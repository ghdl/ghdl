from os import environ
from sys import executable, platform
from io import BytesIO
from json import load as json_load, loads as json_loads, dumps as json_dumps
from pathlib import Path
from subprocess import run as subprocess_run, PIPE
from typing import Optional, Dict, Any, ClassVar
from unittest import TestCase
from pytest import mark

from pyGHDL.lsp.lsp import LanguageProtocolServer, LSPConn


class StrConn:
    __res: str

    def __init__(self):
        self.__res = ""

    def write(self, s: str):
        self.__res += s

    @property
    def res(self) -> str:
        return self.__res


def root_subst(obj, path: str, uri: str):
    """Substitute in all strings of :param obj: @ROOT@ with :param root:
    URI in LSP are supposed to contain an absolute path.  But putting an
    hard absolute path would make the test suite not portable.  So we use
    the metaname @ROOT@ which should be replaced by the root path of the
    test suite.  Also we need to deal with the windows particularity
    about URI."""
    if isinstance(obj, dict):
        for k, v in obj.items():
            if isinstance(v, str):
                if k in ("rootUri", "uri"):
                    assert v.startswith("file://@ROOT@/")
                    obj[k] = f"file://{uri}{v[13:]}"
                elif k in ("rootPath", "message"):
                    obj[k] = v.replace("@ROOT@", path)
            else:
                obj[k] = root_subst(v, path, uri)
        return obj
    elif obj is None or isinstance(obj, (str, int)):
        return obj
    elif isinstance(obj, list):
        return [root_subst(v, path, uri) for v in obj]
    else:
        raise AssertionError(f"root_subst: unhandled type {type(obj)}")


class JSONTest(TestCase):
    _LSPTestDirectory = Path(__file__).parent.resolve()

    subdir: ClassVar[Path]

    def _RequestResponse(self, requestName: str, responseName: Optional[str] = None):
        root = str(self._LSPTestDirectory)
        root_uri = self._LSPTestDirectory.as_uri()
        assert root_uri.startswith("file://")
        root_uri = root_uri[7:]
        requestFile = self._LSPTestDirectory / self.subdir / requestName
        # Convert the JSON input file to an LSP string.
        with requestFile.open("r", encoding="utf-8") as file:
            res = json_load(file)
            res = root_subst(res, root, root_uri)

        conn = StrConn()
        ls = LanguageProtocolServer(None, conn)
        for req in res:
            ls.write_output(req)

        # Run
        p = subprocess_run(
            [executable, "-m", "pyGHDL.cli.lsp"],
            input=conn.res.encode("utf-8"),
            stdout=PIPE,
        )
        self.assertEqual(
            p.returncode,
            0,
            "Language server executable exit with a non-zero return code.",
        )

        if responseName is None:
            return
        responseFile = self._LSPTestDirectory / self.subdir / responseName

        # Check output
        in_io = BytesIO(p.stdout)
        conn = LSPConn(in_io, None)
        ls = LanguageProtocolServer(None, conn)
        with responseFile.open("r", encoding="utf-8") as file:
            ref = json_load(file)
            ref = root_subst(ref, root, root_uri)

        errs = 0
        json_res = []
        for i, r in enumerate(ref):
            rep = ls.read_request()
            if rep is None:
                print("FAIL: number of reply does not match")
                errs += 1
                break

            rep = json_loads(rep)
            json_res.append(rep)
            # 			self.assertEqual(rep, r, f"reply does not match for {requestFile!s}")
            if rep != r:
                print(self.__class__.__name__)
                self._CompareJSONResult(f"[{i}]", r, rep)
                errs += 1

        rep = ls.read_request()
        self.assertIsNone(rep, "Too many replies.")

        if errs != 0:
            print(f"FAILURE between output and {responseFile!s} (for {requestFile!s})")

            requestJSON = Path.cwd() / "request.json"
            resultJSON = Path.cwd() / "result.json"
            print(f"Writing result output to '{resultJSON}' ...")
            with resultJSON.open("w", encoding="utf-8") as f:
                f.write(json_dumps(json_res, indent=2))
                f.write("\n")
            with requestJSON.open("w", encoding="utf-8") as f:
                f.write(json_dumps(res, indent=2))
                f.write("\n")

            self.fail()

    def _CompareJSONResult(self, name: str, ref: Dict[str, Any], res: Dict[str, Any]):
        if isinstance(ref, dict) and isinstance(res, dict):
            for k in ref:
                if k not in res:
                    print(f"{name}.{k} not in the result")
                else:
                    self._CompareJSONResult(f"{name}.{k}", ref[k], res[k])
            for k in res:
                if k not in ref:
                    print(f"{name}.{k} unexpected in the result")
        elif isinstance(ref, str) and isinstance(res, str):
            if res != ref:
                print(f"{name}: mismatch (ref: {ref}, result: {res})")
        elif isinstance(ref, int) and isinstance(res, int):
            if res != ref:
                print(f"{name}: mismatch (ref: {ref}, result: {res})")
        elif isinstance(ref, list) and isinstance(res, list):
            for i in range(max(len(ref), len(res))):
                if i >= len(res):
                    print(f"{name}[{i}]: missing element:")
                    print(f" {ref[i]}")
                elif i >= len(ref):
                    print(f"{name}[{i}]: extra elements")
                else:
                    self._CompareJSONResult(f"{name}[{i}]", ref[i], res[i])
        else:
            print(f"unhandled type {type(ref)} in {name}")


class JSONTest_Single(JSONTest):
    def test_Request_Response(self):
        # Do not run this test, run only within inherited class
        # FIXME: there should be a better way to do this ?
        if self.__class__.__name__ == "JSONTest_Single":
            return
        self._RequestResponse("cmds.json", "replies.json")


class Test001_Simple(JSONTest_Single):
    subdir = Path("001simple")


class Test002_Coverage(JSONTest_Single):
    subdir = Path("002coverage")

class Test003_Errors(JSONTest):
    subdir = Path("003errors")

    def test_Crash1(self):
        self._RequestResponse("crash1.json")

    def test_Crash2(self):
        self._RequestResponse("crash2.json")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


@mark.xfail(platform == "win32" and ("MINGW_PREFIX" not in environ), reason="FIXME")
class Test004_Error_Project(JSONTest_Single):
    subdir = Path("004errprj")


class Test005_Create(JSONTest_Single):
    subdir = Path("005create")


class Test006_Option_Error(JSONTest_Single):
    subdir = Path("006opterr")


class Test007_Error_Project(JSONTest_Single):
    subdir = Path("007errprj")


class Test008_Error_NoFile(JSONTest_Single):
    subdir = Path("008errnofile")


class Test009_ls_122(JSONTest_Single):
    subdir = Path("009ls122")


class Test010_ls_28(JSONTest_Single):
    subdir = Path("010ls28")


class Test011_closediag(JSONTest_Single):
    subdir = Path("011closediag")


class Test012_library(JSONTest_Single):
    subdir = Path("012library")


class Test013_library(JSONTest_Single):
    subdir = Path("013record")
