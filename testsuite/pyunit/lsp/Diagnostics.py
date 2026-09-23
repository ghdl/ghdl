from typing import Any, Dict, List, Optional, Tuple
from unittest import TestCase

from pyGHDL.libghdl import errorout_memory
from pyGHDL.lsp.workspace import Workspace


class FakeDocument:
    """A stand-in for :class:`~pyGHDL.lsp.document.Document`, which needs a parsed file to exist."""

    def __init__(self, uri: str, sfe: int):
        self.uri = uri
        self._fe = sfe


class FakeErrorRecord:
    """The fields of ``errorout_memory.Error_Message`` that :meth:`Workspace.gather_diagnostics` reads."""

    def __init__(self, sfe: int, group: int = errorout_memory.Msg_Single, msgid: int = 3):
        self.id = msgid
        self.group = group
        self.file = sfe
        self.line = 1
        self.offset = 0
        self.length = 1


class DiagnosticsWorkspace(Workspace):
    """A workspace whose documents and published notifications are scripted, so no libghdl state is needed."""

    def __init__(self, documents: Dict[int, FakeDocument]):
        # Deliberately not calling super().__init__: it initializes libghdl and reads a project.
        self._fe_map = documents
        self._diags_set = set()
        self.published: List[Tuple[str, int]] = []

    def sfe_to_document(self, sfe: int) -> FakeDocument:
        return self._fe_map[sfe]

    def publish_diagnostics(self, doc_uri: str, diagnostics: List[Any]) -> None:
        self.published.append((doc_uri, len(diagnostics)))


class Diagnostics(TestCase):
    """Test how :meth:`~pyGHDL.lsp.workspace.Workspace.gather_diagnostics` publishes and withdraws diagnostics."""

    _URI_A = "file:///a.vhdl"
    _URI_B = "file:///b.vhdl"

    def _createWorkspace(self) -> DiagnosticsWorkspace:
        return DiagnosticsWorkspace({1: FakeDocument(self._URI_A, 1), 2: FakeDocument(self._URI_B, 2)})

    def _scriptMessages(self, workspace: DiagnosticsWorkspace, records: List[FakeErrorRecord]) -> None:
        """Let the error memory report the given records for the next gathering."""
        workspace.published.clear()
        errorout_memory.Get_Nbr_Messages = lambda: len(records)
        errorout_memory.Get_Error_Record = lambda index: records[index - 1]
        errorout_memory.Get_Error_Message = lambda index: f"message {index}"
        errorout_memory.Clear_Errors = lambda: None

    def setUp(self) -> None:
        self._originals = {
            name: getattr(errorout_memory, name)
            for name in ("Get_Nbr_Messages", "Get_Error_Record", "Get_Error_Message", "Clear_Errors")
        }

    def tearDown(self) -> None:
        for name, function in self._originals.items():
            setattr(errorout_memory, name, function)

    def test_DiagnosticsArePublishedPerFile(self) -> None:
        workspace = self._createWorkspace()
        self._scriptMessages(workspace, [FakeErrorRecord(1), FakeErrorRecord(2), FakeErrorRecord(2)])

        workspace.gather_diagnostics(None)

        self.assertCountEqual([(self._URI_A, 1), (self._URI_B, 2)], workspace.published)

    def test_DiagnosticsOfAnalyzedDocumentAreWithdrawn(self) -> None:
        """A document whose errors are gone is sent an empty list, even while another document still has some."""
        workspace = self._createWorkspace()
        self._scriptMessages(workspace, [FakeErrorRecord(1), FakeErrorRecord(2)])
        workspace.gather_diagnostics(None)

        # Only b.vhdl reports something now, and a.vhdl is the document that was analyzed.
        self._scriptMessages(workspace, [FakeErrorRecord(2)])
        workspace.gather_diagnostics(workspace.sfe_to_document(1))

        self.assertIn((self._URI_A, 0), workspace.published)
        self.assertIn((self._URI_B, 1), workspace.published)

    def test_DiagnosticsOfAnotherDocumentAreWithdrawn(self) -> None:
        """Diagnostics published for a document are withdrawn once an analysis no longer reports them."""
        workspace = self._createWorkspace()
        self._scriptMessages(workspace, [FakeErrorRecord(1), FakeErrorRecord(2)])
        workspace.gather_diagnostics(None)

        self._scriptMessages(workspace, [])
        workspace.gather_diagnostics(None)

        self.assertCountEqual([(self._URI_A, 0), (self._URI_B, 0)], workspace.published)

    def test_UnchangedDiagnosticsAreNotWithdrawn(self) -> None:
        """A document that still has diagnostics is not sent an empty list on top of them."""
        workspace = self._createWorkspace()
        self._scriptMessages(workspace, [FakeErrorRecord(1)])
        workspace.gather_diagnostics(None)

        self._scriptMessages(workspace, [FakeErrorRecord(1)])
        workspace.gather_diagnostics(workspace.sfe_to_document(1))

        self.assertEqual([(self._URI_A, 1)], workspace.published)

    def test_RelatedInformationKeepsTheAnalyzedDocument(self) -> None:
        """A related message names its own file, and does not become the document the round was about."""
        workspace = self._createWorkspace()
        self._scriptMessages(
            workspace,
            [
                FakeErrorRecord(1, errorout_memory.Msg_Main),
                FakeErrorRecord(2, errorout_memory.Msg_Related),
            ],
        )
        workspace.gather_diagnostics(workspace.sfe_to_document(2))

        # a.vhdl carries the diagnostic, b.vhdl only the related location - so b.vhdl has none of its own.
        self.assertCountEqual([(self._URI_A, 1), (self._URI_B, 0)], workspace.published)

    def test_ClosedDocumentIsNotWithdrawnTwice(self) -> None:
        """Closing a document withdraws its diagnostics, and the next analysis does not repeat that."""
        workspace = self._createWorkspace()
        self._scriptMessages(workspace, [FakeErrorRecord(1)])
        workspace.gather_diagnostics(None)

        workspace.published.clear()
        workspace.rm_document(self._URI_A)
        self._scriptMessages(workspace, [])
        workspace.gather_diagnostics(None)

        self.assertEqual([], workspace.published)
