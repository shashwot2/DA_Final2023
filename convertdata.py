import csv

txt_file = 'enterprise_projects.txt'
csv_file = 'enterprise_projects.csv'

column_names = [
    'url', 'project_id', 'sdtc', 'mcpc', 'mcve', 'star_number', 'commit_count',
    'files', 'lines', 'pull_requests', 'github_repo_creation', 'earliest_commit',
    'most_recent_commit', 'committer_count', 'author_count', 'dominant_domain',
    'dominant_domain_committer_commits', 'dominant_domain_author_commits',
    'dominant_domain_committers', 'dominant_domain_authors', 'cik', 'fg500',
    'sec10k', 'sec20f', 'project_name', 'owner_login', 'company_name', 'owner_company', 'license'
]

with open(txt_file, 'r', encoding="utf-8") as infile, open(csv_file, 'w', newline='', encoding="utf-8") as outfile:
    writer = csv.writer(outfile)

    writer.writerow(column_names)

    reader = csv.reader(infile, delimiter="\t")

    for row in reader:
        writer.writerow(row)

print(f"The file {txt_file} has been converted to {csv_file}")
