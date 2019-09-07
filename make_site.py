import os, string

def pretty(name):
    return string.capwords(name.replace('_', ' '))

las = []
for f in os.listdir('docs'):
    if f.endswith('.html') or f.endswith('summary'):
        continue
    las.append(f)
las.sort()

for la in las:
    index_filename = "docs/{}/index.html".format(la)
    with open(index_filename, "w") as index_file:
        index_file.write("""<html>
<head>
    <title>Level The Playing Field</title>
</head>
<body>
<table>
<tr>
""")
        for type in ('_pupil_funding_vs_fsm.png', '_pupil_funding_vs_outturn.png', '_pupil_funding_vs_year.png', '_school_funding_vs_size.png'):
            index_file.write('<td><a href="{}{}"><img src="{}{}" width="250" height="250"></a></td>\n'.format(la, type, la, type))
        index_file.write("""
</tr>
</table>
</body>
""")

with open("docs/la_table.html", "w") as la_table:
    la_table.write("""<html>
<head>
    <title>Level The Playing Field</title>
</head>
<body>
<table>
    <thead>
        <tr>
            <th>Local Authority</th>
            <th></th>
            <th></th>
            <th></th>
            <th></th>
        </tr>
    </thead>
""")
    for la in las:
        la_table.write('<tr>\n')
        la_table.write('<td>{}</td>\n'.format(pretty(la)))
        for type in ('_pupil_funding_vs_fsm.png', '_pupil_funding_vs_outturn.png', '_pupil_funding_vs_year.png', '_school_funding_vs_size.png'):
            la_table.write('<td><a href="{}/{}{}"><img src="{}/{}{}" width="250" height="250"></a></td>\n'.format(la, la, type, la, la, type))
        la_table.write('</tr>\n')
    la_table.write("""
</table>
</body>
""")