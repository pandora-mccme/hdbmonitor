## db-monitor: data consistency monitoring for PostgreSQL.

This simple tool periodically runs provided SQL-queries and alerts DBAs about failed checks via Telegram.

We believe such an instrument may help those who are administrating one or several databases on a single Linux server to find data malformations early. Hence promptly notify developers about possible bugs and other oddities like inaccurate manual data manipulation.

And we hope it allows more structured monitoring process than arbitrary one built on shell scripts.

All unnecessary restrictions (PostgreSQL, Telegram, Linux) are coming from our limited resources and current conditions. They are not ideological, so we will be glad to accept pull requests broadening domain of the tool.

### Configuration and usage.

To be run `dbmonitor` requires a configuration directory. By default it will be searched in current directory under name `monitor`, but you may pass it as an option (`--dir <path>` or `-D <path>`).

Structure of the directory is the following (in terms of `ls -R` command output):
```
<config-dir>:
<database1-dir>
<database2-dir>
<database3-dir>
...

<config-dir>/<database1-dir>:
conf.dhall check1.sql check2.sql check3.sql ...
```

`conf.dhall` is a configuration file for single database. Name of the file is fixed. It stores connection string, list of Telegram handles of alert-receivers (channels), string which should prefix messages and some other default settings.

Example contents of `conf.dhall`:
```
{ connection = "host=localhost user=user port=5432 dbname=postgres password=password"
, users = ["login1", "login2", "login3"]
, preamble = "Big Brother is watching your data:"
, frequency = 1 -- Default period in minutes between checks.
, assertion = "null" -- Default assertion made against result of any query.
}
```

Files of checks are real SQL files with optional comments required for setting check behavior. These files may have arbitrary names, even extension is unchecked.

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
SELECT count(id) FROM table1 WHERE obligatory_field IS NULL;
```

If `assertion` or `frequency` comment is omitted, default value applies.
These comments must satisfy the following regular expression: `^--\s*[:field:]\s*=\s*[:value:]\s*$`.

Assertions can be made on SQL side or on Haskell side and it's more native to do them on SQL side, so `assertion` field may have only one of these values -- `null`, `not null`, `true`, `false`, `zero`.
Frequency is expected to be positive integer. I.e any value less than 1 will be treated as 1, fractional numbers will be considered as parse errors.

Incorrect assertion or syntactically wrong query will result in messages to maintainers.

Telegram token is expected to be stored in environmental variable `TG_TOKEN`. You can pass name of the variable as an option. `--token <variable-name>` or `-T <variable-name>`.

**Recommended usage:**
Running -- `dbmonitor &`. It's highly recommended to put `@reboot dbmonitor` line in your `crontab`.

**Options Reference:**

* `--token <variable-name>` or `-T <variable-name>` -- name of environmental variable where Telegram token is stored.
* `--dir <path>` or `-D <path>` -- path to configuration directory.
* `--help` or `-h` -- see options reference in usual optparse-applicative format.

### Behavior details

* Job queue created by tool does not persist anywhere. Hence on restart all possible monitoring events will happen.
* If job is modified, old instance of it's runnable representation is killed and replaced. Hence after change monitoring event will immediately happen.

**_Issues and pull requests are welcome._**
