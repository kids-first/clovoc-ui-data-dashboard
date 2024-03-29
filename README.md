# CLOVoc UI Data Dashboard

The CLOVoc UI Data Dashboard pulls JSON resources from a FHIR API(s), tabluates against custom table descriptions, and renders a table view.

## Quickstart

1. Make sure R is installed on your local machine or remote server where the dashboard is deployed.

2. Clone this repository:

```
$ git clone git@github.com:kids-first/clovoc-ui-data-dashboard.git
$ cd clovoc-ui-data-dashboard
```

3. Create a `.env` file in the root directory:

```
FHIR_API_COOKIE="YOUR-FHIR-API-COOKIE"
```

or export an environmental variable in your command line interface:

```
$ export FHIR_API_COOKIE="YOUR-FHIR-API-COOKIE"
```

4. Run the script below. Once successfully run, the data dashboard is available at [http://localhost:3838](http://localhost:3838):

```
$ ./scripts/entrypoint.sh
```

![Dashboard](./docs/dashboard.png)

## Development

- To contribute to this repository, please follow [Google's R Stype Guide](https://google.github.io/styleguide/Rguide.html).
- Run the linter `./scripts/prettify.sh /path/to/code` before pushing commits.
