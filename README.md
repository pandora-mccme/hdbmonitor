## db-monitor: data consistency monitoring for PostgreSQL.

This simple tool periodically runs provided SQL-queries and alerts DBAs about failed checks via Telegram.
It's highly configurable and tracks configuration changes.

We believe such an instrument may help those who are administrating one or several databases on a single Linux server to find data malformations early. Hence promptly notify developers about possible bugs and other oddities like inaccurate manual data manipulation. Other use case -- executing periodic actions like vacuuming on database with failure alerts.

And we hope it allows more structured monitoring process than arbitrary one built on shell scripts.

All unnecessary restrictions (PostgreSQL, Telegram, Linux with `libc6` >= 2.29) are coming from our limited resources and current conditions. They are not ideological, so we will be glad to accept pull requests broadening domain of the tool.

### Configuration and usage.

To be run `dbmonitor` requires a configuration directory. By default it will be searched in current directory under name `.monitor`, but you may pass it as an option (`--dir <path>` or `-D <path>`).

Structure of the directory is the following (in terms of `ls -R` command output):
```
<config-dir>:
<database1-dir>
<database2-dir>
<database3-dir>
...

<config-dir>/<database1-dir>:
conf.dhall check1.sql check2.sql check3.sql job1.sql ...
```

`conf.dhall` is a configuration file for single database. Name of the file is fixed. It stores connection string, list of Telegram IDs of alert-receivers (channels) and some other default settings.

Example contents of `conf.dhall`:
```
{ connection = "host=localhost user=user port=5432 dbname=postgres password=password"
, channels = [-1001408342381, ...]
, frequency = 1 -- Default period in minutes between checks.
, assertion = "null" -- Default assertion made against result of any query.
}
```

Files of checks and jobs are plain SQL files with optional comments required for setting check behavior. These files may have arbitrary names with .sql extension.

Proper check file contents may look like that:
```
-- frequency = 30
-- assertion = null
SELECT id FROM table1 WHERE NOT id = ANY(SELECT DISTINCT reference_column FROM table2);
```

or
```
SELECT id FROM table1 WHERE NOT id = ANY(SELECT DISTINCT reference_column FROM table2);
```

or
```
-- frequency = 10
-- assertion = zero
-- description = Some text
-- description = Some other text
SELECT count(id) FROM table1 WHERE obligatory_field IS NULL;
```

If `assertion` or `frequency` comment is omitted, default value applies.
These comments must satisfy the following regular expression: `^--\s*[:field:]\s*=\s*[:value:]\s*$`. `description` lines are optional and will appear in alerts.

Complex assertions can be made on SQL side or on Haskell side and it's more native to do them on SQL side, so `assertion` field may have only one of these values -- `null`, `not null`, `true`, `false`, `zero`, `resultless`. Last value is reserved for jobs.

Frequency is expected to be positive integer. I.e any value less than 1 will be treated as 1, decimal numbers will be truncated.

Incorrect assertion or syntactically wrong query will result in messages to maintainers.

Telegram token is expected to be stored in environmental variable `TG_TOKEN`. You can pass name of the variable as an option. `--token <variable-name>` or `-T <variable-name>`.

Log is written to stdout with immediate buffer flushes.

**Options Reference:**

* `--token <variable-name>` or `-T <variable-name>` -- name of environmental variable where Telegram token is stored.
* `--dir <path>` or `-D <path>` -- path to configuration directory.
* `--help` or `-h` -- see options reference in usual optparse-applicative format.

### Installation.

Tool is now available at https://github.com/viviag/db-monitor/pkgs/container/db-monitor.

### Behavior details

#### Persistence

Tool is designed to be stable under changes of configuration.

This stability is achived in a following manner:
* If a new database directory is created in `.monitor`, it's automatically tracked.
* System tracks changes in check files and automatically reload them.
* New check files are automatically tracked and run as jobs.
* On removal of check file tool stops executing related job. Same for database directories - directory deletion causes monitor for this directory to stop.
* `conf.dhall` changes are tracked. If config is invalid, old jobs are removed, but database monitor is not stopped, after config fix everything will work again.
* On removal of `.monitor` directory tool dies with error message.

#### Other implementation details

* We do not track hidden files (prefixed with dot). Also we do not care about plain files on a level of database directories but traverse directories on level of check files.
* If assertion cannot be parsed from `conf.dhall` or check file, it is treated as `not null` instead of throwing error.
* Database queries are based on `hasql` library, which has quite strict control over what query is expected to return. So you may encounter some unfamiliar errors in Telegram. In all cases we know they are reasonable against provided assertions. Also usage of this library as a backend limits us to PostgeSQL databases. It's possible to extend to other backends.
* File watch is implemented over `inotify`. It's a Linux kernel subsystem, so it's why tool is Linux-specific. It's possible to extend at least to MacOS using `kqueue` backend.
* Hopefully you will find our logs detailed but not floody.

### Telegram caveat.

We expect users to be familiar with SQL, but there is a caveat in Telegram.

Usually monitoring channels must be private. It's impossible to send message to private channel by it's name, that's why we restricted channel field of `conf.dhall` to chat ids. Id of a private channel may be extracted from URL in a web version of Telegram.

### Attribution.

This tool was initially written for HyperMath team at Moscow Center for Continuous Mathematical Education.
Systems maintaning there include https://7.math.ru, https://mathtraining.ru and several services for developing interactive texts in school-level mathematics.

Code ownership and authorship are my own.

**_Issues and pull requests are welcome._**
