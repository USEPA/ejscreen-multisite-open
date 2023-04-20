name: trytesting
# also see 
# https://github.com/r-lib/actions/blob/v2/examples/check-standard.yaml
on:
  push:
    # branches: [ "main "]

jobs:
  test:
    runs-on: ubuntu-latest
permissions:
  issues: write
steps:
  - uses: actions/checkout@v3
# with:
  # repo-token: ${{ secrets.GITHUB_TOKEN }}

  issue-message: "Congratulations this action was attempted"
  pr-message: "Congratulations this was attempted"

  
  # 
  # - name: Show testthat output
  # if: always()
  # run: |
  #   ## --------------------------------------------------------------------
  # find ${{ runner.temp }}/package -name 'testthat.Rout*' -exec cat '{}' \; || true
  # shell: bash
