import React from 'react';

import { formatBuild } from './builds';
import styles from './Demo.module.css';

import type { Build } from './builds';
import type { DiagnosticsSnapshot } from './permalink';

const REPORTS_URL = 'https://github.com/makenowjust-labs/recheck/discussions/categories/reports';
const REPORTS_EMAIL = 'make.just.on@gmail.com';
const SPONSORS_URL = 'https://github.com/sponsors/makenowjust';

const Row: React.FC<{ title: string; children?: React.ReactNode }> = ({ title, children }) => (
  <div className="row">
    <div className="col col--3">
      <h4>{title}</h4>
    </div>
    <div className="col col--9">{children}</div>
  </div>
);

const Badge: React.FC<{ kind: string; children: string }> = ({ kind, children }) => (
  <p>
    <span className={`badge badge--${kind}`}>{children}</span>
  </p>
);

const CopyButton: React.FC<{ text: string }> = ({ text }) => {
  const [copied, setCopied] = React.useState(false);
  const copy = async () => {
    await navigator.clipboard.writeText(text);
    setCopied(true);
    setTimeout(() => setCopied(false), 1500);
  };
  return (
    <button className="button button--sm button--secondary" onClick={copy}>
      {copied ? 'Copied' : 'Copy'}
    </button>
  );
};

const Loading: React.FC = () => (
  <div className="row">
    <div className="col col--12 text--center">
      <div className={styles.demoResultLoading} role="status" aria-label="Loading">
        <span />
        <span />
        <span />
        <span />
      </div>
    </div>
  </div>
);

const Hotspot: React.FC<{ diagnostics: Extract<DiagnosticsSnapshot, { status: 'vulnerable' }> }> = ({
  diagnostics,
}) => {
  const spots = [];
  let index = 0;
  for (const { start, end, temperature } of diagnostics.hotspot) {
    if (index < start) {
      spots.push(<span key={index}>{diagnostics.source.substring(index, start)}</span>);
    }
    const className = temperature === 'heat' ? styles.demoResultHotspotHeat : styles.demoResultHotspotNormal;
    spots.push(
      <span key={start} className={className}>
        {diagnostics.source.substring(start, end)}
      </span>,
    );
    index = end;
  }
  if (index < diagnostics.source.length) {
    spots.push(<span key={index}>{diagnostics.source.substring(index)}</span>);
  }
  return (
    <pre>
      <code>/{spots}/</code>
    </pre>
  );
};

type Props = {
  title: string;
  result: DiagnosticsSnapshot | null;
  time: number;
  log: string[];
  build: Build;
  date: string | null;
  permalink: string | null;
  children?: React.ReactNode;
};

const DemoResult: React.FC<Props> = ({ title, result, time, log, build, date, permalink, children }) => {
  const logBottomRef = React.useRef<HTMLDivElement>(null);
  React.useEffect(() => {
    logBottomRef.current?.scrollIntoView({ behavior: 'smooth', block: 'nearest', inline: 'start' });
  }, [log]);

  const logRow =
    log.length > 0 ? (
      <Row title="Log">
        <pre className={styles.demoResultLog}>
          {log.join('\n')}
          <div ref={logBottomRef} />
        </pre>
      </Row>
    ) : null;

  const rows = [];
  if (result === null) {
    rows.push(<Loading key="loading" />);
  } else {
    rows.push(
      <Row title="Input" key="input">
        <pre>{result.status === 'invalid' ? result.input : `/${result.source}/${result.flags}`}</pre>
      </Row>,
      <Row title="Time" key="time">
        <p>{(time / 1000).toFixed(2)} s</p>
      </Row>,
    );
    switch (result.status) {
      case 'invalid':
        rows.push(
          <Row title="Status" key="status">
            <Badge kind="secondary">invalid</Badge>
          </Row>,
        );
        break;
      case 'unknown':
        rows.push(
          <Row title="Status" key="status">
            <Badge kind="secondary">unknown</Badge>
          </Row>,
          <Row title="Error Kind" key="error-kind">
            <p>{result.error.kind}</p>
          </Row>,
        );
        if (result.error.message !== undefined) {
          rows.push(
            <Row title="Error Message" key="error-message">
              <p>{result.error.message}</p>
            </Row>,
          );
        }
        if (result.checker) {
          rows.push(
            <Row title="Checker" key="checker">
              <Badge kind="secondary">{result.checker}</Badge>
            </Row>,
          );
        }
        break;
      case 'safe':
        rows.push(
          <Row title="Status" key="status">
            <Badge kind="info">safe</Badge>
          </Row>,
          <Row title="Checker" key="checker">
            <Badge kind="secondary">{result.checker}</Badge>
          </Row>,
        );
        break;
      case 'vulnerable':
        rows.push(
          <Row title="Status" key="status">
            <Badge kind="danger">vulnerable</Badge>
          </Row>,
          <Row title="Complexity" key="complexity">
            <Badge kind={result.complexity.type === 'exponential' ? 'danger' : 'warning'}>
              {result.complexity.summary}
            </Badge>
          </Row>,
          <Row title="Attack String" key="attack">
            <pre>
              <code>{result.attackPattern}</code>
            </pre>
          </Row>,
          <Row title="Hotspot" key="hotspot">
            <Hotspot diagnostics={result} />
          </Row>,
          <Row title="Checker" key="checker">
            <Badge kind="secondary">{result.checker}</Badge>
          </Row>,
        );
        break;
    }
    if (permalink !== null) {
      rows.push(
        <Row title="Permalink" key="permalink">
          <p>
            <a href={permalink}>{permalink.length > 80 ? `${permalink.slice(0, 80)}…` : permalink}</a>{' '}
            <CopyButton text={permalink} />
          </p>
          {result.status === 'vulnerable' ? (
            <div className="alert alert--info margin-top--sm" role="note">
              <p>
                This permalink records the result and the parameters, so you can cite it in a vulnerability report.
                Once the vulnerability is fixed and disclosed, please let us know in <a href={REPORTS_URL}>Reports</a>{' '}
                or by email to <a href={`mailto:${REPORTS_EMAIL}`}>{REPORTS_EMAIL}</a>. We are going to list projects
                using recheck and vulnerabilities found with it on this site.
              </p>
              <p className="margin-bottom--none">
                If recheck helped you, consider supporting it on <a href={SPONSORS_URL}>GitHub Sponsors</a>.
              </p>
            </div>
          ) : null}
        </Row>,
      );
    }
  }

  return (
    <div className="card margin-vert--lg">
      <div className="card__header">
        <h3>{title}</h3>
      </div>
      <div className="card__body">
        <div className="container">
          {children}
          {rows}
          {logRow}
        </div>
      </div>
      {result !== null ? (
        <div className={`card__footer ${styles.demoResultMeta}`}>
          recheck {formatBuild(build)}
          {date !== null ? ` · ${new Date(date).toLocaleString()}` : null}
        </div>
      ) : null}
    </div>
  );
};

export default DemoResult;
