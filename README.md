# CLOVoc UI Data Dashboard

The CLOVoc UI Data Dashboard pulls JSON resources from a FHIR API(s), tabluates against custom table descriptions, and renders a table view.

## Quickstart

1. Make sure R is installed on your local machine or remote server where the tabulator is deployed.

2. Clone this repository:

```
$ git clone git@github.com:kids-first/clovoc-ui-data-dashboard.git
$ cd clovoc-ui-data-dashboard
```

3. Put a FHIR cookie to the `fhir_api_cookie` variable of your api tabulation plugin.

4. Run the script:

```
$ ./scripts/entrypoint.sh
```

## Development

- To contribute to this repository, please follow [Google's R Stype Guide](https://google.github.io/styleguide/Rguide.html).
- Run the linter `./scripts/prettify.sh /path/to/code` before pushing commits.
