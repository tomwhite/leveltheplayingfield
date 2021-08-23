import os, string

# These should match constants in utils.R
LATEST_YEAR = '2019-20'
LATEST_NUM_PUPILS_YEAR = '2020-21'
LATEST_OUTTURN_YEAR = '2019-20'
LATEST_SUPPORT_CATEGORY_YEAR = '2019'
LATEST_FSM_YEAR = '2020-21'

def pretty(name):
    return string.capwords(name.replace('_', ' '))

las = []
for f in os.listdir('docs_aug_2021/local_authorities'):
    las.append(f)
las.sort()

for la in las:
    index_filename = "docs_aug_2021/local_authorities/{}/index.html".format(la)
    with open(index_filename, "w") as index_file:
        index_file.write("""<html>
<head>
    <title>Level The Playing Field</title>
</head>
<body>
<table>
<tr>
""")
        for type in ('_primary_pupil_funding_vs_outturn_{}.png'.format(LATEST_OUTTURN_YEAR),
                     '_primary_pupil_funding_vs_year.png',
                     '_primary_school_funding_vs_size_{}.png'.format(LATEST_NUM_PUPILS_YEAR)):
            index_file.write('<td><a href="{}{}"><img src="{}{}" width="250" height="250"></a></td>\n'.format(la, type, la, type))
        index_file.write("""
</tr>
</table>
</body>
""")

with open("docs_aug_2021/la_table.html", "w") as la_table:
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
        for type in ('_primary_pupil_funding_vs_outturn_{}.png'.format(LATEST_OUTTURN_YEAR),
                     '_primary_pupil_funding_vs_year.png',
                     '_primary_school_funding_vs_size_{}.png'.format(LATEST_NUM_PUPILS_YEAR),
                     '_primary_size_vs_fsm_{}.png'.format(LATEST_NUM_PUPILS_YEAR)):
            la_table.write('<td><a href="local_authorities/{}/{}{}"><img src="local_authorities/{}/{}{}" width="250" height="250"></a></td>\n'.format(la, la, type, la, la, type))
        la_table.write('</tr>\n')
    la_table.write("""
</table>
</body>
""")

with open("docs_aug_2021/index.html", "w") as la_table:
    la_table.write("""<html>
<head>
    <title>Level The Playing Field Reports</title>
</head>
<body>

<p>See <a href="index2.html">other index</a>, <a href="posts.html">custom reports for blog posts</a></p>

<table border="1">
    <thead>
    <tr>
        <th>Local Authority</th>

        <th colspan="6">Population Trends</th>
        <th colspan="6">School Funding</th>
        <th colspan="4">School Finance</th>
        <th colspan="3">School Support Categories</th>
        <th colspan="2">FSM Rates</th>
        <th colspan="1">Welsh Language Provision</th>
    </tr>

    <tr>
        <th></th>

        <th>Chart</th>
        <th>Chart</th>
        <th>Map</th>
        <th>Map (Pri)</th>
        <th>Map (Sec)</th>
        <th>Map (Thr)</th>

        <th>Chart 1</th>
        <th>Chart 2</th>
        <th>Pri</th>
        <th>Sec</th>
        <th>Map (Pri)</th>
        <th>Map (Sec)</th>

        <th>Chart</th>
        <th>Map</th>
        <th>Pri</th>
        <th>Sec</th>

        <th>Map</th>
        <th>Pri</th>
        <th>Sec</th>

        <th>Chart</th>
        <th>Map</th>

        <th>Map</th>
    </tr>
    </thead>
    <tr>
        <td>All Wales (by school type)</td>

        <td>-</td>
        <td>-</td>
        <td><a href="wales/all_schools_occupancy_2019-20.html">Y</a> (<a href="wales/all_schools_occupancy_alt_2019-20.html">alt</a>)</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td><a href="wales/all_schools_outturn_surplus_or_deficit_2018-19.html">Y</a> (<a href="wales/all_schools_outturn_surplus_or_deficit_alt_2018-19.html">alt</a>)</td>
        <td>-</td>
        <td>-</td>

        <td><a href="wales/all_schools_support_category_2019.html">Y</a> (<a href="wales/all_schools_support_category_alt_2019.html">alt</a>)</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td>-</td>

        <td><a href="wales/all_schools_language_2018-19.html">Y</a> (<a href="wales/all_schools_language_alt_2018-19.html">alt</a>)</td>
    </tr>
    <tr>
        <td>All Wales (by school size)</td>

        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td><a href="wales/primary_occupancy_with_school_size_2019-20.html">Y</a> (<a href="wales/primary_occupancy_with_school_capacity_2019-20.html">cap</a>)</td>
        <td><a href="wales/secondary_occupancy_with_school_size_2019-20.html">Y</a> (<a href="wales/secondary_occupancy_with_school_capacity_2019-20.html">cap</a>)</td>
        <td><a href="wales/through_occupancy_with_school_size_2019-20.html">Y</a> (<a href="wales/through_occupancy_with_school_capacity_2019-20.html">cap</a>)</td>

        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td>-</td>

        <td>-</td>
    </tr>
    <tr>
        <td>All Wales (by LA)</td>

        <td>-</td>
        <td>-</td>
        <td><a href="wales/all_schools_occupancy_with_la_2019-20.html">Y</a></td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td>-</td>
        <td><a href="wales/primary_per_pupil_funding_2019-20.html">Y</a></td>
        <td><a href="wales/secondary_per_pupil_funding_2019-20.html">Y</a></td>

        <td>-</td>
        <td><a href="wales/all_schools_outturn_surplus_or_deficit_with_la_2018-19.html">Y</a></td>
        <td>-</td>
        <td>-</td>

        <td><a href="wales/all_schools_support_category_with_la_2019.html">Y</a></td>
        <td>-</td>
        <td>-</td>

        <td>-</td>
        <td><a href="wales/all_schools_fsm_with_la_2018-19.html">Y</a></td>

        <td><a href="wales/all_schools_language_with_la_2018-19.html">Y</a></td>
    </tr>
""")

    for la in las:
        la_table.write("""    <tr>
        <td>{}</td>

        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a> (<a href="{}">cap</a>)</td>
        <td><a href="{}">Y</a> (<a href="{}">cap</a>)</td>
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

        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>
        <td><a href="{}">Y</a></td>

        <td><a href="{}">Y</a></td>
    </tr>
""".format(pretty(la),

           "local_authorities/{}/{}_total_population_vs_year.png".format(la, la),
           "local_authorities/{}/{}_total_population_with_age_vs_year.png".format(la, la),
           "local_authorities/{}/{}_all_schools_occupancy_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),
           "local_authorities/{}/{}_primary_occupancy_with_school_size_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),
           "local_authorities/{}/{}_primary_occupancy_with_school_capacity_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),
           "local_authorities/{}/{}_secondary_occupancy_with_school_size_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),
           "local_authorities/{}/{}_secondary_occupancy_with_school_capacity_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),

           "local_authorities/{}/{}_all_schools_delegation_rate_vs_year.png".format(la, la),
           "local_authorities/{}/{}_all_schools_delegated_school_budget_per_pupil_vs_year.png".format(la, la),
           "local_authorities/{}/{}_primary_delegated_school_budget_per_pupil_vs_year.png".format(la, la),
           "local_authorities/{}/{}_secondary_delegated_school_budget_per_pupil_vs_year.png".format(la, la),
           "local_authorities/{}/{}_primary_per_pupil_funding_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),
           "local_authorities/{}/{}_secondary_per_pupil_funding_{}.html".format(la, la, LATEST_NUM_PUPILS_YEAR),

           "local_authorities/{}/{}_primary_pupil_funding_vs_pupil_outturn_{}.png".format(la, la, LATEST_OUTTURN_YEAR),
           "local_authorities/{}/{}_all_schools_outturn_surplus_or_deficit_{}.html".format(la, la, LATEST_OUTTURN_YEAR),
           "local_authorities/{}/{}_primary_pupil_outturn_vs_year.png".format(la, la),
           "local_authorities/{}/{}_secondary_pupil_outturn_vs_year.png".format(la, la),

           "local_authorities/{}/{}_all_schools_support_category_{}.html".format(la, la, LATEST_SUPPORT_CATEGORY_YEAR),
           "local_authorities/{}/{}_primary_support_category_vs_year.png".format(la, la),
           "local_authorities/{}/{}_secondary_support_category_vs_year.png".format(la, la),

           "local_authorities/{}/{}_primary_pupil_funding_vs_fsm_{}.png".format(la, la, LATEST_FSM_YEAR),
           "local_authorities/{}/{}_all_schools_fsm_{}.html".format(la, la, LATEST_FSM_YEAR),

           "local_authorities/{}/{}_all_schools_language_{}.html".format(la, la, LATEST_YEAR)))

    la_table.write("""
</table>
</body>
""")