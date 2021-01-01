import svgwrite

dwg = svgwrite.Drawing('logo.svg', profile='tiny')

_size = 20
_fill = svgwrite.rgb(50, 50, 50, '%')
_stroke_width = 2

_GHDL = [
    [
        "01110",
        "10001",
        "10000",
        "10111",
        "10001",
        "10001",
        "01111"
    ],
    [
        "10001",
        "10001",
        "10001",
        "11111",
        "10001",
        "10001",
        "10001",
    ],
    [
        "11100",
        "10010",
        "10001",
        "10001",
        "10001",
        "10010",
        "11100"
    ],
    [
        "10000",
        "10000",
        "10000",
        "10000",
        "10000",
        "10000",
        "11111"
    ]
]

for idl, _letter in enumerate(_GHDL):
    for idy, _row in enumerate(_letter):
        for idx, val in enumerate(_row):
            print(idl, idx, idy)
            if val == '1':
                dwg.add(dwg.rect(
                    (idl*_size*6 + idx*_size+_stroke_width, idy*_size+_stroke_width),
                    (_size-2*_stroke_width, _size-2*_stroke_width),
                    fill=_fill,
                ))

dwg.save()
