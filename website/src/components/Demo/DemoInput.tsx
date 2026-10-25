import React from 'react';

import NumberInput from '../form/NumberInput';
import Select from '../form/Select';
import TextArea from '../form/TextArea';
import { formatBuild } from './builds';
import styles from './Demo.module.css';
import { params } from './params';

import type { Build } from './builds';
import type { Param } from './params';

const ParamInput: React.FC<Param> = ({ name, defaultValue, type }) => {
  if (type === 'duration') {
    return <NumberInput name={name} defaultValue={defaultValue} step={100} />;
  }
  if (type === 'float') {
    return <NumberInput name={name} defaultValue={defaultValue} step={0.001} />;
  }
  if (type === 'integer') {
    return <NumberInput name={name} defaultValue={defaultValue} step={1} />;
  }
  return <Select name={name} values={type.select} defaultValue={String(defaultValue)} />;
};

const DemoParams: React.FC = () => (
  <details>
    <summary>Parameters</summary>

    <div className="container">
      {params.map((param) => (
        <div className="row margin-vert--sm" key={param.name}>
          <div className="col col--3">
            <h4>{`${param.title}${param.type === 'duration' ? ' (ms)' : ''}`}</h4>
          </div>
          <div className="col col--2 col--offset-1">
            <ParamInput {...param} />
          </div>
        </div>
      ))}
    </div>
  </details>
);

const LOCAL_NOTE = 'Everything runs in your browser. The regular expression and the results are never sent to a server.';

const LocalNote: React.FC = () => {
  const [open, setOpen] = React.useState(false);
  return (
    <div className={styles.localNote}>
      <span className={styles.localNoteText}>{LOCAL_NOTE}</span>
      <button
        type="button"
        className={styles.localNoteIcon}
        aria-label={LOCAL_NOTE}
        aria-expanded={open}
        onClick={() => setOpen((open) => !open)}
      >
        i
      </button>
      {open ? (
        <div className={styles.localNotePopover} role="tooltip">
          {LOCAL_NOTE}
        </div>
      ) : null}
    </div>
  );
};

type Props = {
  check: () => void;
  cancel: () => void;
  running: boolean;
  builds: Build[];
  defaultBuild: Build;
};

const DemoInput: React.FC<Props> = ({ check, cancel, running, builds, defaultBuild }) => (
  <div className="card margin-vert--lg">
    <div className="card__header">
      <h3>RegExp</h3>
    </div>
    <div className="card__body">
      <div className="container">
        <div className="row row--no-gutters margin-top-sm">
          <div className="col col--12">
            <TextArea name="input" defaultValue="/^(a|a)*$/" placeholder="/^(a|a)*$/" />
          </div>
        </div>

        <div className={`margin-top--sm ${styles.actions}`}>
          <LocalNote />
          <div className={styles.version}>
            <Select
              name="version"
              values={builds.map(formatBuild)}
              defaultValue={formatBuild(defaultBuild)}
              aria-label="recheck version"
              disabled={running}
            />
          </div>
          <div>
            {running ? (
              <button className="button button--secondary" onClick={() => cancel()}>
                Cancel
              </button>
            ) : null}
            <button className="button button--primary" onClick={() => check()} disabled={running}>
              Check
            </button>
          </div>
        </div>

        <div className="row margin-top--sm row--no-gutters">
          <div className="col col--12">
            <DemoParams />
          </div>
        </div>
      </div>
    </div>
  </div>
);

export default DemoInput;
