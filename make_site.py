import os, string

def pretty(name):
    return string.capwords(name.replace('_', ' '))

las = []
for f in os.listdir('docs/local_authorities'):
    las.append(f)
las.sort()

for la in las:
    index_filename = "docs/local_authorities/{}/index.html".format(la)
    with open(index_filename, "w") as index_file:
        index_file.write("""<html>
<head>
    <title>Level The Playing Field</title>
</head>
<body>
<table>
<tr>
""")
        for type in ('_primary_pupil_funding_vs_fsm.png', '_primary_pupil_funding_vs_outturn.png', '_primary_pupil_funding_vs_pupil_outturn.png', '_primary_pupil_funding_vs_year.png', '_primary_school_funding_vs_size.png', '_primary_support_category_vs_year.png', '_secondary_support_category_vs_year.png'):
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
        for type in ('_primary_pupil_funding_vs_fsm.png', '_primary_pupil_funding_vs_outturn.png', '_primary_pupil_funding_vs_pupil_outturn.png', '_primary_pupil_funding_vs_year.png', '_primary_school_funding_vs_size.png', '_primary_support_category_vs_year.png', '_secondary_support_category_vs_year.png'):
            la_table.write('<td><a href="local_authorities/{}/{}{}"><img src="local_authorities/{}/{}{}" width="250" height="250"></a></td>\n'.format(la, la, type, la, la, type))
        la_table.write('</tr>\n')
    la_table.write("""
</table>
</body>
""")

with open("docs/index.html", "w") as la_table:
    la_table.write("""<html>
<head>
    <title>Level The Playing Field</title>
</head>
<body>

<p>See <a href="index2.html">other index</a></p>

<table border="1">
    <thead>
    <tr>
        <th>Local Authority</th>

        <th colspan="3">Population Trends</th>
        <th colspan="4">School Finance</th>
        <th colspan="3">School Support Categories</th>
        <th colspan="1">Free School Meal Rates</th>
        <th colspan="3">Welsh Language Provision</th>
    </tr>

    <tr>
        <th></th>

        <th>Chart</th>
        <th>Chart</th>
        <th>Map</th>

        <th>Chart</th>
        <th>Map</th>
        <th>Pri</th>
        <th>Sec</th>

        <th>Map</th>
        <th>Pri</th>
        <th>Sec</th>

        <th>Chart</th>

        <th>Map</th>
        <th>Chart</th>
        <th>Chart</th>
    </tr>
    </thead>
""")

    for la in las:
        la_table.write("""    <tr>
        <td>{}</td>

        <td>-</td>
        <td>-</td>
        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>
        <td>-</td>
        <td>-</td>
    </tr>
""".format(pretty(la),
           "local_authorities/{}/{}_all_schools_occupancy.html".format(la, la),
           "local_authorities/{}/{}_primary_pupil_funding_vs_pupil_outturn.png".format(la, la),
           "local_authorities/{}/{}_all_schools_outturn_surplus_or_deficit.html".format(la, la),
           "local_authorities/{}/{}_primary_school_vs_budget_outturn_change.png".format(la, la),
           "local_authorities/{}/{}_secondary_school_vs_budget_outturn_change.png".format(la, la),
           "local_authorities/{}/{}_all_schools_support_category.html".format(la, la),
           "local_authorities/{}/{}_primary_support_category_vs_year.png".format(la, la),
           "local_authorities/{}/{}_secondary_support_category_vs_year.png".format(la, la),
           "local_authorities/{}/{}_primary_pupil_funding_vs_fsm.png".format(la, la),
           "local_authorities/{}/{}_all_schools_language.html".format(la, la)))

    la_table.write("""
</table>
</body>
""")