.. # This file provides the following shields: travis-ci appveyor release
   # license mailing gitter issues-new issues-open issues-closed issues-pr
   # issues-pr-closed github gh-logo

.. # Use http://b64.io/ to encode any image to base64. Then replace `/` with
   # `%2F` and `+` with `%2B` (or use http://meyerweb.com/eric/tools/dencoder/).
   # Beware that `?logo=data:image/png;base64,` must also be converted to
   # percent encoding so that the URL is properly parsed.

.. only:: html

   .. |SHIELD:travis-ci| image:: https://img.shields.io/travis/ghdl/ghdl/master.svg?style=flat&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAAIP0lEQVR42qVXaUyUWRaFycyP%2BTVLemaSSfrPJN3JZJKOPyY6uBLTQgGlUMVeFHuxFaioKGpcAFFQUaOigAsmo7ghikZtxZVFCCKgqOi4ICoqqLjghqjcOeem%2B5sQFZnuL3l5VV%2B9d%2B9595577iuX%2F%2FcZNWrUX7y8vMyJiYmbpk6d2pKamto5bdq0LnxuT05OPhgUFOQcOXLkP35a7%2Bvr6%2FKLnmXLlun87t27X02YMKFkzpw5va2trXL37l158OCBPHz4UEdXV5fcu3dPbt26Jdu3b%2B%2Fz8%2FNrM5vNf%2BNeAPxlIHAi06xZs7rOnj0rL168kPb2dnn8%2BLE8evToo0EgHPfv35e1a9cKQOT9LKc2m03nyMhIv%2FXr18uzZ8%2F0pDU1NXLz5k0xTzSLycs0cHibBClhBOTJkycKqKmpSZCyGkTv10N27unpqfPw4cNDSkpK5OrVqzSmAHbu3CnXr1%2BXhIQECQkNkeCQYHEkxAPQRHEmJ0taWposWbJEOjs7dX1bW5s0NjYyEtW0OXny5KGBMJlMf1ixYsUHOqYRGmPYi4qK5PLly5KUlCSLFi0S5FqCAaSutk6io6MlMzNTtmzZIkwX9zASdXV10tLSQhALaRtEHdy5iLhYrdbbP53gxo0baqy7u1tWrlxJAErA7sfdYrFYNBINZxv0XUdHh5SXl8uhQ4eMPbW1tfqZqQwODv7rF0%2F%2FPZ6GhoZ%2B5v3AgQNKqqdPnyoBly9fztMxJeSFAeDcuXN8p9Wxf%2F9%2BBfH8%2BXMFcP78eSMlWN%2F8SafDhg0zPqOmG2%2Ffvi137tzRkF%2B6dEny8vLE399fUA1y%2BvRpGtMT%2B1n8JMQWKhUVFQTKiKlzkE7XHjlyRJqbm7lWI3n06NG%2B0aNHfzvAOVDq7Obm9mcIzIbS0lK5cuWKVFVVyerVq2Xv3r2aw5kzZ0pubq6cOnWKNa8njYyKlKRkpyBlPL2yPiUlhfzQVJ04cUI2bdrE1CmI%2Bvp6Wbp06bkRI0ZYBoAA4u9JIIAxGL9jxw5GgeRjCjT8GzdulM2bN2s5btu2TcJsNgmz2yU8IkIBHz58WIm4atUq6enpoS21iagyJbTL1BEYQe8zALi7u%2BdiAxcYg8iZd4a2srJST%2B9wOMTb25tk0gqYiPLjmDRpkkYhJCRE4uPjBeUmBw8e1JJ9%2Ffo108GDGLYJJjw8XAwAY8eOzedLIuZCkoY1Tzbv2rVLEV%2B4cIE5NpSOPCHrORh%2BpoX7GDVqx5kzZ2Tfvn2yZ88eVUVKOG1zMAoRiJoBADnJpEHmiMTh5prqaimHgeLiYs2p0%2BkkETUCXt5eUD8dVDod0A7OjIhGYcGCBVJQUCBlZWVMD21yZjQ1Aj4%2BPv8DMGbMmDCemDJKR%2BPGjRNfhNgeES7hkREML0tIAfiYzWK3h6kA2TBjPUNMkPjdKgmJiRIHdXTEIV0%2B3poWdEqBrGvYs7Oz5dixY0zhfwwAULGvwsLCVMvt4XaJdcRKQlIinahTdEE60FxDyQDOl1Whsrt48WI1Ghpqkxjso3OSkpGaN2%2Be5OfnGwNiRBUlSQnMNqAScEpvi9UiMbGxgj4uqFcl2ty5c2XGjBn6vbCwkGlibbM0WWpaEdAurX0eonhLMbmjJM7JyWFZ0h4JrLYCAwM%2FIJpRsOPqAlTqHPX%2FDXKHU4T2BAYFfsAirQA%2B1HNupNo1NTeR1RzqnJrBCGVkZGiX5MO%2BsBm86evrE9wjpL%2B%2Fn6%2B1R0yfPp0cqfxICRGuDoweEKkxJiaGZaSsfvnypTK8EyLCfK4tLEClPKQ9rqFCsu45U%2FO19gODgmXm7NkQsDICpQ0dGzZsoLBRP%2FrHjx%2F%2Fb5gwnFeDkeunTJnye7J0zZo1zB1DrBtfvXql9ezp4SFubv8Ci09rmeGmQxAsNQWB2xMdajpGurmRI4Zz2KBdKiJ1gNVyLC4uztUFOR6NcJsJBMz%2FHUrwPRWQqnfy5EluplHKL0nIk%2FIdDWl%2Bjx8%2FLu%2Ffv9eajgV33rx5w9Qoy69du8Zy43qt%2B6ysLA7VDcj%2BVj09yuMbzgDBLvhb3AHuV1ZWybp168h8CpPm8O3bt4IOSSlWIwsXLmQFkB%2FMNVlNpSQXWBX8TcO9detW2b17t8yfP5%2BVwshSrlnmjoEXTwWT4moyeda0tl6mkGhIESYC4elY6zCcg%2Fe5MDhPAJZEZYSUgLORd4Cjc%2B4FoBxGjaCQogyWMEVIbYP0f%2FzMXWD8KKvV8i4gIOA8ygcLEzhQQrEwvBgdsBzt%2BSLCeJcRYYulLCvDS3eXwvFSSO4lKF4l0lCB0%2B9CNPORqgpwIq3Fw8PDirT%2F8xOuSUbTgO%2FZ2Yva6uvr0GIb0UrvoSM%2B0ZGenk6RgSL6yAQQc5y7u5ghq%2F4BARJqs1ELlAe9vb1IUTd7B%2FZ3Qaq9nHDugVI3D3YdcwWzv0Y5uqI%2FpOMKTgYbTKYexMXHSaIz6bOD94OJCDfTwT7CywrbMWz24MLbi1R8DdJ%2B5NsQJSxIxeIqCExAVFRU%2B8WLF6WosEiNhIbZ6GRIIzo2WiIiI9gvMEeqPCMFJS6DPUQGyfwNkHZAlhnmtww3DTlTkj%2FliNrPvvHFqFihmC5DfXDyP1nQAZO%2BfFIoXxDb82cdE3gwOiLELuPHxje4c%2Bi1ziZPUzxuPjTyScMcDvCBf1IKCwpBQn90UYfEo2qiYqKVjAGBAc%2B9fXxqQdL0H1V3aBFAGeqMlpnKjmgQD62UTtiy%2FSwWtmYqm5IUXKFeMM%2FsomV2u%2F3vtAEuufysByfgRJX8DshTPU2mPPy7yQBBYzGWgC%2FVkOSXWVmZZHwnSPoD%2FlOmodXSsWFjsOe%2FCAcrQqMD2awAAAAASUVORK5CYII%3D
      :target: https://travis-ci.org/ghdl/ghdl
      :alt: Build status by Travis-CI

.. |SHIELD:appveyor| image:: https://ci.appveyor.com/api/projects/status/7hx4kfbhidv06sin?svg=true
   :target: https://ci.appveyor.com/project/tgingold/ghdl-psgys/branch/master
   :alt: Build status by AppVeyor

.. |SHIELD:release| image:: https://img.shields.io/github/release/ghdl/ghdl.svg?label=latest%20release&style=flat
   :target: https://github.com/ghdl/ghdl/releases
   :alt: Latest release

.. |SHIELD:code-lic| image:: https://img.shields.io/badge/code%20license-GPLv2-bd0000.svg?style=flat
   :target: Licenses.html
   :alt: GNU General Public License 2

.. |SHIELD:doc-lic| image:: https://img.shields.io/badge/doc%20license-CC--BY--SA--4.0-aab2ab.svg?style=flat
   :target: Licenses.html
   :alt: Creative Commons Attribution-ShareAlike 4.0

.. |SHIELD:gitter| image:: https://img.shields.io/gitter/room/ghdl1/Lobby.svg?colorB=4cb696
   :target: https://gitter.im/ghdl1/Lobby
   :alt: Talk to us on Gitter

.. |SHIELD:issues-new| image:: https://img.shields.io/badge/new-issue-yellowgreen.svg?style=flat
   :target: https://github.com/ghdl/ghdl/issues/new
   :alt: Open new issue at GitHub

.. |SHIELD:new-pr| image:: https://img.shields.io/badge/new-pull--request-yellowgreen.svg?style=flat
   :target: https://github.com/ghdl/ghdl/compare/
   :alt: Open new Pull Request (PR) at GitHub

.. |SHIELD:bug-report| image:: https://img.shields.io/badge/new-bug--report-yellowgreen.svg?style=flat
   :target: https://github.com/ghdl/ghdl/issues/new?template=bug_report.md
   :alt: Open new bug report at GitHub

.. |SHIELD:feature-request| image:: https://img.shields.io/badge/new-feature--request-yellowgreen.svg?style=flat
   :target: https://github.com/ghdl/ghdl/issues/new?template=feature_request.md
   :alt: Open new feature request at GitHub

.. |SHIELD:issues-open| image:: https://img.shields.io/github/issues/ghdl/ghdl.svg
   :target: https://github.com/ghdl/ghdl/issues
   :alt: Open issues

.. |SHIELD:issues-closed| image:: https://img.shields.io/github/issues-closed-raw/ghdl/ghdl.svg?style=flat
   :target: https://github.com/ghdl/ghdl/issues?q=is%3Aissue+is%3Aclosed
   :alt: Closed issues

.. |SHIELD:issues-pr| image:: https://img.shields.io/github/issues-pr/ghdl/ghdl.svg?style=flat
   :target: https://github.com/ghdl/ghdl/pulls
   :alt: Open pull requests

.. |SHIELD:issues-pr-closed| image:: https://img.shields.io/github/issues-pr-closed-raw/ghdl/ghdl.svg?style=flat
   :target: https://github.com/ghdl/ghdl/pulls
   :alt: Closed pull requests

.. |SHIELD:contributors| image:: https://img.shields.io/github/contributors/ghdl/ghdl.svg?style=flat
   :target: https://github.com/ghdl/ghdl/graphs/contributors
   :alt: Contributors

.. only:: html

   .. |SHIELD:gh-logo| image:: https://img.shields.io/badge/-ghdl/ghdl-323131.svg?style=flat&logo=data%3Aimage%2Fpng%3Bbase64%2CiVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAMAAABEpIrGAAABKVBMVEX%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FOe9X6AAAAYnRSTlMAAQIDBAcIDA0PEBESFBUWGBkeHyEoMTIzNDU4OTw%2BP0JGWVxeX2BiY2RlZ2hvc3R1eHp%2Bio%2BXm52epKmqq62usLe5wcLDxMnQ0dLW2tvc3t%2Fh4uPo6uvs8fLz9fb3%2Bvv8%2FsuNaVkAAAF7SURBVHgBjdHZW9NAFAXwE0iElCARZCkuIotEUFyQBSJERBEILaAo6d6e%2F%2F%2BPcO58aZslD%2Fxe5mHuPfOdbzA06R1e1sn65aE3ibw5v8OBjj%2BHNHuvx5Tenp1av2bOdSJkKWKBaAmxhYiFogVoEzckw%2BModXcckryZgDigsgLLu1MVT09V1TvPwgqVAyhlinmpMjsCZWRWCsxTlAEEFIvIWKQIALdLsYaMNYqui02KPw4ynN8Um%2FAp1pGzTuEj1Ek2cmz9dogmlVsUuKXSBEUFBaoUoKihQI0iPqaQMxWvnlOsImeV4hz7FBcmMswLin0sU9tBxg61ZYw3qB25SHhyRK0xDuySX179INtfN8qjOvz5u6BNjbsAplvsbVm%2FqAQGFOMb%2B1rTULZJPnsqf%2FMC2kv2bUOYZ%2BR3lLzPr0ehPWbszITmVsn3GCqRWtVFbKZC%2Fvz45tOj1EBlBgPOCUUpOXDiIMF4%2Bzc98G%2FDQNrY1tW9Bc26v%2Fowhof6D6AkqSgsdGGuAAAAAElFTkSuQmCC
      :target: https://www.github.com/ghdl/ghdl
      :alt: Source Code on GitHub
