# Security Policy

## Supported versions

Security reports are accepted for the current `main` branch and the latest
released version of `retraction`.

| Version | Supported |
| --- | --- |
| Latest release | Yes |
| `main` | Yes |
| Older releases | Best effort |

## Reporting a vulnerability

Please do not report security vulnerabilities in public issues.

Preferred reporting path:

1. Use GitHub's private vulnerability reporting or security advisory workflow if
   it is available for this repository.
2. If that is not available, email the maintainer at
   <a.sofimahmudi@gmail.com>.

Include:

- A description of the vulnerability.
- Steps to reproduce it.
- The affected package version or commit.
- Whether the issue involves document parsing, network requests to a source API,
  the local snapshot cache, or generated reports.
- Any known mitigations.

The maintainer will acknowledge reports as soon as practical, investigate the
issue, and coordinate a fix and disclosure timeline. Public disclosure should
wait until a fix or mitigation is available, unless there is an overriding user
safety reason to disclose sooner.

## Scope

In scope:

- Code execution, injection, path traversal, or unsafe file handling when
  parsing documents and bibliographies (for example `.docx`, PDF, XML, BibTeX,
  or RIS input).
- Unsafe handling of untrusted metadata or API responses (JSON, XML).
- Unsafe handling of the local snapshot cache or generated HTML and Markdown
  reports.
- Exposure of credentials, tokens, or private document contents.

Out of scope:

- Vulnerabilities only present in third-party services or data sources queried
  by `retraction`.
- Disagreements about whether a specific reference is retracted, without a
  security impact.
- Spam, social-engineering campaigns, or denial-of-service reports that do not
  identify a package vulnerability.
