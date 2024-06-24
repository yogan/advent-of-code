#!/bin/bash
sha=$1
names=$(git diff --name-only "$sha"^ "$sha" | grep -v README)

changes_ci=$(echo "$names" | grep -c "^.github/workflows")
changes_docker=$(echo "$names" | grep -c "^Dockerfile")
changes_scripts=$(echo "$names" | grep -c "^scripts")
changes_templates=$(echo "$names" | grep -c "^templates")
changes_vim=$(echo "$names" | grep -c "^vim")
changes_2015=$(echo "$names" | grep -c "^2015")
changes_2016=$(echo "$names" | grep -c "^2016")
changes_2020=$(echo "$names" | grep -c "^2020")
changes_2021=$(echo "$names" | grep -c "^2021")
changes_2023=$(echo "$names" | grep -c "^2023")

# Docker changes are intentionally not counted as "any changes"
# Changes to the Dockerfile shall only trigger a rebuild of the Docker image.
# There is no point in running any solution or template tests in that case.
any_changes=$((changes_ci + \
    changes_scripts + \
    changes_templates + \
    changes_vim + \
    changes_2015 + \
    changes_2016 + \
    changes_2020 + \
    changes_2021 + \
    changes_2023))

echo "changes-ci=$changes_ci" | tee -a "$GITHUB_OUTPUT"
echo "changes-docker=$changes_docker" | tee -a "$GITHUB_OUTPUT"
echo "changes-scripts=$changes_scripts" | tee -a "$GITHUB_OUTPUT"
echo "changes-templates=$changes_templates" | tee -a "$GITHUB_OUTPUT"
echo "changes-vim=$changes_vim" | tee -a "$GITHUB_OUTPUT"
echo "changes-2015=$changes_2015" | tee -a "$GITHUB_OUTPUT"
echo "changes-2016=$changes_2016" | tee -a "$GITHUB_OUTPUT"
echo "changes-2020=$changes_2020" | tee -a "$GITHUB_OUTPUT"
echo "changes-2021=$changes_2021" | tee -a "$GITHUB_OUTPUT"
echo "changes-2023=$changes_2023" | tee -a "$GITHUB_OUTPUT"
echo "any-changes=$any_changes" | tee -a "$GITHUB_OUTPUT"
