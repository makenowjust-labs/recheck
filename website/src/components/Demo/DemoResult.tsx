import React from 'react';

import styles from './Demo.module.css';
import { formatBuild } from './permalink';

import type { Build, Snapshot } from './permalink';

export const REPORTS_URL = 'https://github.com/makenowjust-labs/recheck/discussions/categories/reports';
export const REPORTS_EMAIL = 'make.just.on@gmail.com';

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

export const CopyButton: React.FC<{ text: string }> = ({ text }) => {
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

const Hotspot: React.FC<{ diagnostics: Extract<Snapshot['result'], { status: 'vulnerable' }> }> = ({
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

const reportSnippet = (
  diagnostics: Extract<Snapshot['result'], { status: 'vulnerable' }>,
  build: Build,
  permalink: string,
): string =>
  [
    `Detected by recheck ${formatBuild(build)}: ${permalink}`,
    `RegExp: /${diagnostics.source}/${diagnostics.flags}`,
    `Complexity: ${diagnostics.complexity.summary} (${diagnostics.checker})`,
    `Attack string: ${diagnostics.attack.pattern}`,
  ].join('\n');

const Report: React.FC<{
  diagnostics: Extract<Snapshot['result'], { status: 'vulnerable' }>;
  build: Build;
  permalink: string;
}> = ({ diagnostics, build, permalink }) => {
  const snippet = reportSnippet(diagnostics, build, permalink);
  return (
    <Row title="Report">
      <pre>
        <code>{snippet}</code>
      </pre>
      <p>
        <CopyButton text={snippet} />
      </p>
      <p>
        Found a ReDoS vulnerability with recheck? Please share it in the{' '}
        <a href={REPORTS_URL}>Reports</a> discussions, or by email to{' '}
        <a href={`mailto:${REPORTS_EMAIL}`}>{REPORTS_EMAIL}</a> if it cannot be public yet. Mentioning recheck and
        this permalink in your advisory also helps the project.
      </p>
    </Row>
  );
};

type Props = {
  title: string;
  result: Snapshot['result'] | null;
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
        if ('message' in result.error) {
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
              <code>{result.attack.pattern}</code>
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
    rows.push(
      <Row title="Build" key="build">
        <p>{formatBuild(build)}</p>
      </Row>,
    );
    if (date !== null) {
      rows.push(
        <Row title="Date" key="date">
          <p>{new Date(date).toLocaleString()}</p>
        </Row>,
      );
    }
    if (permalink !== null) {
      rows.push(
        <Row title="Permalink" key="permalink">
          <p>
            <a href={permalink}>{permalink.length > 80 ? `${permalink.slice(0, 80)}…` : permalink}</a>{' '}
            <CopyButton text={permalink} />
          </p>
        </Row>,
      );
      if (result.status === 'vulnerable') {
        rows.push(<Report key="report" diagnostics={result} build={build} permalink={permalink} />);
      }
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
    </div>
  );
};

export default DemoResult;
