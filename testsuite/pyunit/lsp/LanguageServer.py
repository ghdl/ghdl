from os import environ
from sys import executable, platform
from io import BytesIO
from json import load as json_load, loads as json_loads, dumps as json_dumps
from pathlib import Path
from urllib.parse import quote
from subprocess import run as subprocess_run, PIPE
from typing import Optional
from unittest import TestCase
from pytest import mark

from pyGHDL.lsp.lsp import LanguageProtocolServer, LSPConn


class StrConn:
    __res: str

    def __init__(self):
        self.__res = ""

    def write(self, s):
        self.__res += s

    @property
    def res(self):
        return self.__res


def show_diffs(name, ref, res):
    if isinstance(ref, dict) and isinstance(res, dict):
        for k in ref:
            if k not in res:
                print("{}.{} not in the result".format(name, k))
            else:
                show_diffs("{}.{}".format(name, k), ref[k], res[k])
        for k in res:
            if k not in ref:
                print("{}.{} unexpected in the result".format(name, k))
    elif isinstance(ref, str) and isinstance(res, str):
        if res != ref:
            print("{}: mismatch (ref: {}, result: {})".format(name, ref, res))
    elif isinstance(ref, int) and isinstance(res, int):
        if res != ref:
            print("{}: mismatch (ref: {}, result: {})".format(name, ref, res))
    elif isinstance(ref, list) and isinstance(res, list):
        for i in range(max(len(ref), len(res))):
            if i >= len(res):
                print("{}[{}]: missing element:".format(name, i))
                print(" {}".format(res[i]))
            elif i >= len(ref):
                print("{}[{}]: extra elements".format(name, i))
            else:
                show_diffs("{}[{}]".format(name, i), ref[i], res[i])
    else:
        print("unhandle type {} in {}".format(type(ref), name))


def root_subst(obj, path, uri):
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
                    obj[k] = "file://" + uri + v[13:]
                elif k in ("rootPath", "message"):
                    obj[k] = v.replace("@ROOT@", path)
            else:
                obj[k] = root_subst(v, path, uri)
        return obj
    elif obj is None or isinstance(obj, (str, int)):
        return obj
    elif isinstance(obj, list):
        res = []
        for v in obj:
            res.append(root_subst(v, path, uri))
        return res
    else:
        raise AssertionError("root_subst: unhandled type {}".format(type(obj)))


class JSONTest(TestCase):
    _LSPTestDirectory = Path(__file__).parent.resolve()

    subdir = None

    def _RequestResponse(self, requestName: str, responseName: Optional[str] = None):
        root = str(self._LSPTestDirectory)
        root_uri = self._LSPTestDirectory.as_uri()
        assert root_uri.startswith("file://")
        root_uri = root_uri[7:]
        requestFile = self._LSPTestDirectory / self.subdir / requestName
        # Convert the JSON input file to an LSP string.
        with requestFile.open("r") as file:
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
        with responseFile.open("r") as file:
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
            # 			self.assertEqual(rep, r, "reply does not match for {!s}".format(requestFile))
            if rep != r:
                print(self.__class__.__name__)
                show_diffs("[{}]".format(i), r, rep)
                errs += 1

        rep = ls.read_request()
        self.assertIsNone(rep, "Too many replies.")

        if errs != 0:
            print("FAILURE between output and {!s} (for {!s})".format(responseFile, requestFile))
            print("Writing result output to result.json")
            with open("result.json", "w") as f:
                f.write(json_dumps(json_res, indent=2))
                f.write("\n")
            with open("request.json", "w") as f:
                f.write(json_dumps(res, indent=2))
                f.write("\n")

            self.fail()


class Test001_Simple(JSONTest):
    subdir = Path("001simple")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test002_Coverage(JSONTest):
    subdir = Path("002coverage")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test003_Errors(JSONTest):
    subdir = Path("003errors")

    def test_Crash1(self):
        self._RequestResponse("crash1.json")

    def test_Crash2(self):
        self._RequestResponse("crash2.json")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


@mark.xfail(platform == "win32" and ("MINGW_PREFIX" not in environ), reason="FIXME")
class Test004_Error_Project(JSONTest):
    subdir = Path("004errprj")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test005_Create(JSONTest):
    subdir = Path("005create")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test006_Option_Error(JSONTest):
    subdir = Path("006opterr")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test007_Error_Project(JSONTest):
    subdir = Path("007errprj")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")


class Test008_Error_NoFile(JSONTest):
    subdir = Path("008errnofile")

    def test_Request_Response(self):
        self._RequestResponse("cmds.json", "replies.json")
