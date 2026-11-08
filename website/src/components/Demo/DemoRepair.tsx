import Link from '@docusaurus/Link';
import React from 'react';

import Select from '../form/Select';
import styles from './Demo.module.css';
import { Badge, CopyButton, Loading, Row } from './DemoResult';

import type { Match } from 'recheck';
import type { Keep, RepairSnapshot } from './permalink';

export const KEEP_LABELS: Record<Keep, string> = {
  all: 'The match and every capture',
  named: 'The match and the named captures',
  'match-range': 'The match',
  match: 'Only whether it matches',
};

// A string input is shown as a JSON string, which escapes the control characters, and units as code points.
const formatInput = (input: string | number[]): string =>
  typeof input === 'string'
    ? JSON.stringify(input)
    : `[${input.map((u) => `0x${u.toString(16).toUpperCase()}`).join(', ')}]`;

const formatCapture = (capture: [number, number] | null): string =>
  capture === null ? 'unset' : `${capture[0]}–${capture[1]}`;

const captureName = (index: number): string => (index === 0 ? 'Match' : `Capture ${index}`);

const MatchTable: React.FC<{ kept: number[]; original: Match | null; repaired: Match | null }> = ({
  kept,
  original,
  repaired,
}) => (
  <table>
    <thead>
      <tr>
        <th />
        <th>Original</th>
        <th>Repaired</th>
      </tr>
    </thead>
    <tbody>
      {original === null || repaired === null ? (
        <tr>
          <td>Match</td>
          <td>{original === null ? 'no match' : formatCapture(original[0] ?? null)}</td>
          <td>{repaired === null ? 'no match' : formatCapture(repaired[0] ?? null)}</td>
        </tr>
      ) : (
        kept.map((index, i) => (
          <tr key={index}>
            <td>{captureName(index)}</td>
            <td>{formatCapture(original[i] ?? null)}</td>
            <td>{formatCapture(repaired[i] ?? null)}</td>
          </tr>
        ))
      )}
    </tbody>
  </table>
);

const complexityKind = (type: string): string =>
  type === 'exponential' ? 'danger' : type === 'polynomial' ? 'warning' : 'success';

type RowsProps = {
  repair: RepairSnapshot;
  flags: string;
  /** Checks the repaired regular expression, which is absent for a recorded repair. */
  check?: (input: string) => void;
};

const RepairRows: React.FC<RowsProps> = ({ repair, flags, check }) => {
  const { keep, result, time } = repair;
  const rows = [
    <Row title="Keep" key="keep">
      <p>{KEEP_LABELS[keep]}</p>
    </Row>,
    <Row title="Time" key="time">
      <p>{(time / 1000).toFixed(2)} s</p>
    </Row>,
  ];
  switch (result.status) {
    case 'safe':
      rows.push(
        <Row title="Status" key="status">
          <Badge kind="info">safe</Badge>
        </Row>,
      );
      break;
    case 'unknown':
      rows.push(
        <Row title="Status" key="status">
          <Badge kind="secondary">unknown</Badge>
        </Row>,
      );
      if (result.error === null) {
        rows.push(
          <Row title="Error Kind" key="error-kind">
            <p>No edit makes the regular expression better.</p>
          </Row>,
        );
      } else {
        rows.push(
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
      }
      break;
    case 'repaired': {
      const repaired = `/${result.repaired}/${flags}`;
      const { comparison } = result;
      rows.push(
        <Row title="Status" key="status">
          <Badge kind="success">repaired</Badge>
        </Row>,
        <Row title="Repaired" key="repaired">
          <pre>{repaired}</pre>
          <p className={styles.repairActions}>
            <CopyButton text={repaired} />
            {check !== undefined ? (
              <button className="button button--sm button--secondary" onClick={() => check(repaired)}>
                Check it
              </button>
            ) : null}
          </p>
        </Row>,
        <Row title="Complexity" key="complexity">
          <Badge kind={complexityKind(result.complexity.type)}>{result.complexity.summary}</Badge>
        </Row>,
        <Row title="Comparison" key="comparison">
          {comparison.kind === 'equivalent' ? (
            <>
              <Badge kind="success">equivalent</Badge>
              <p>It matches the same as the original on every input, with the same kept captures.</p>
            </>
          ) : comparison.kind === 'unknown' ? (
            <>
              <Badge kind="secondary">unknown</Badge>
              <p>The comparison does not support these regular expressions.</p>
            </>
          ) : (
            <>
              <Badge kind="warning">different</Badge>
              <p>They match differently on the input below, the shortest such input. Read it before you replace the original.</p>
              <pre>{formatInput(comparison.input)}</pre>
              <MatchTable kept={result.kept} original={comparison.original} repaired={comparison.repaired} />
            </>
          )}
        </Row>,
      );
      break;
    }
  }
  return <>{rows}</>;
};

type Props = {
  recorded?: boolean;
  running: boolean;
  repair: RepairSnapshot | null;
  flags: string;
  start?: () => void;
  cancel?: () => void;
  check?: (input: string) => void;
};

const DemoRepair: React.FC<Props> = ({ recorded = false, running, repair, flags, start, cancel, check }) => (
  <div className="card margin-vert--lg">
    <div className="card__header">
      <h3>
        Repair{' '}
        {recorded ? (
          <span className="badge badge--secondary" title="Shown from the permalink">
            recorded
          </span>
        ) : null}
      </h3>
    </div>
    <div className="card__body">
      <div className="container">
        {!recorded ? (
          <div className={`margin-bottom--md ${styles.repairControls}`}>
            <label htmlFor="keep">Keep</label>
            <div className={styles.repairKeep}>
              <Select
                id="keep"
                name="keep"
                values={Object.keys(KEEP_LABELS)}
                labels={KEEP_LABELS}
                defaultValue="all"
                disabled={running}
              />
            </div>
            <div>
              {running ? (
                <button className="button button--secondary" onClick={() => cancel?.()}>
                  Cancel
                </button>
              ) : null}
              <button className="button button--primary" onClick={() => start?.()} disabled={running}>
                Repair
              </button>
            </div>
          </div>
        ) : null}
        {!recorded && repair === null && !running ? (
          <p>
            The repair suggests a better regular expression, keeping what you choose of the matches (see{' '}
            <Link to="/docs/usage/repair">Repair</Link>).
          </p>
        ) : null}
        {running ? <Loading /> : null}
        {!running && repair !== null ? <RepairRows repair={repair} flags={flags} check={check} /> : null}
      </div>
    </div>
  </div>
);

export default DemoRepair;
