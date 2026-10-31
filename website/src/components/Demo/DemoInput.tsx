import Link from '@docusaurus/Link';
import React from 'react';
import { useFormContext } from 'react-hook-form';

import NumberInput from '../form/NumberInput';
import Select from '../form/Select';
import TextArea from '../form/TextArea';
import { formatBuild } from './builds';
import styles from './Demo.module.css';
import { DEFAULT_DIALECT, dialectOf, paramsOf, selectValues } from './params';

import type { Build } from './builds';
import type { Param } from './params';

const ParamInput: React.FC<{ param: Param; dialect: string }> = ({ param, dialect }) => {
  const { name, defaultValue, type } = param;
  const { getValues, setValue } = useFormContext();
  const values = selectValues(param, dialect);
  const byDialect = typeof type === 'object' && 'selectByDialect' in type;

  // A value of another dialect is replaced with this dialect's default.
  React.useEffect(() => {
    if (byDialect && !values.includes(String(getValues(name)))) {
      setValue(name, values[0]);
    }
  }, [byDialect, dialect]);

  if (type === 'duration') {
    return <NumberInput name={name} defaultValue={defaultValue} step={100} />;
  }
  if (type === 'float') {
    return <NumberInput name={name} defaultValue={defaultValue} step={0.001} />;
  }
  if (type === 'integer') {
    return <NumberInput name={name} defaultValue={defaultValue} step={1} />;
  }
  return <Select name={name} values={values} defaultValue={byDialect ? values[0]! : String(defaultValue)} />;
};

const DemoParams: React.FC<{ build: Build }> = ({ build }) => {
  const { watch } = useFormContext();
  const dialect = dialectOf({ dialect: watch('dialect') }, build);
  return (
    <details>
      <summary>Parameters</summary>

      <div className="container">
        {paramsOf(build, dialect)
          .filter(({ name }) => name !== 'dialect')
          .map((param) => (
            <div className="row margin-vert--sm" key={param.name}>
              <div className="col col--3">
                <h4>{`${param.title}${param.type === 'duration' ? ' (ms)' : ''}`}</h4>
              </div>
              <div className="col col--2 col--offset-1">
                <ParamInput param={param} dialect={dialect} />
              </div>
            </div>
          ))}
      </div>
    </details>
  );
};

const DemoDialect: React.FC<{ build: Build; disabled: boolean }> = ({ build, disabled }) => {
  const { watch } = useFormContext();
  const param = paramsOf(build).find(({ name }) => name === 'dialect');
  if (param === undefined) return null;
  const dialect = String(watch('dialect') ?? param.defaultValue);
  return (
    <>
      <div className={`margin-top--sm ${styles.dialect}`}>
        <label htmlFor="dialect">{param.title}</label>
        <div className={styles.dialectSelect}>
          <Select
            id="dialect"
            name="dialect"
            values={selectValues(param, DEFAULT_DIALECT)}
            defaultValue={String(param.defaultValue)}
            disabled={disabled}
          />
        </div>
      </div>
      <div className={styles.dialectFlags}>
        <Link to={`/docs/usage/dialects/${dialect}#flags-and-units`}>Flags of {dialect}</Link>
      </div>
    </>
  );
};

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
  build: Build;
  defaultBuild: Build;
};

const DemoInput: React.FC<Props> = ({ check, cancel, running, builds, build, defaultBuild }) => (
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

        <DemoDialect build={build} disabled={running} />

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
            <DemoParams build={build} />
          </div>
        </div>
      </div>
    </div>
  </div>
);

export default DemoInput;
