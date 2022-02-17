import React from 'react';
import { useForm, FormProvider } from 'react-hook-form';
import ReactLoading from 'react-loading';
import * as recheck from 'recheck';

import styles from './Demo.module.css';
import NumberInput from './form/NumberInput';
import Select from './form/Select';
import TextArea from './form/TextArea';

import type { Diagnostics } from 'recheck';

type ParamType =
  | 'duration' | 'float' | 'integer'
  | {
    select: string[];
  };

type Param = {
  title: string;
  name: string;
  defaultValue: any;
  type: ParamType;
};

type DemoParamsProps = {
  params: Param[];
};

const DemoParams: React.VFC<DemoParamsProps> = ({ params }) => {
  const paramsComponents = params.map(({ title, name, defaultValue, type }) => {
    let inputComponent = null;

    if (type === 'duration') {
      inputComponent = (
        <div className="col col--2 col--offset-1">
          <NumberInput name={name} defaultValue={defaultValue} step={100} />
        </div>
      );
    }

    if (type === 'float') {
      inputComponent = (
        <div className="col col--2 col--offset-1">
          <NumberInput name={name} defaultValue={defaultValue} step={0.001} />
        </div>
      );
    }

    if (type === 'integer') {
      inputComponent = (
        <div className="col col--2 col--offset-1">
          <NumberInput name={name} defaultValue={defaultValue} step={1} />
        </div>
      );
    }

    if (typeof type === 'object' && typeof type.select === 'object') {
      inputComponent = (
        <div className="col col--2 col--offset-1">
          <Select name={name} values={type.select} defaultValue={defaultValue} />
        </div>
      );
    }

    return (
      <div className="row margin-vert--sm" key={name}>
        <div className="col col--3"><h4>{`${title}${type === 'duration' ? ' (ms)' : ''}`}</h4></div>
        {inputComponent}
      </div>
    );
  });

  return (
    <details>
      <summary>Parameters</summary>

      <div className="container">
        {paramsComponents}
      </div>
    </details>
  );
};

type DemoInputProps = {
  check: () => void;
  cancel: () => void;
  status: 'init' | 'running' | 'done';
}

const DemoInput: React.VFC<DemoInputProps> = ({ check, cancel, status }) => {
  const params: Param[] = [
    {
      "name": "checker",
      "title": "Checker",
      "defaultValue": "auto",
      "type": {
        "select": [
          "auto",
          "automaton",
          "fuzz"
        ]
      }
    },
    {
      "name": "logger",
      "title": "Logger",
      "defaultValue": "off",
      "type": {
        "select": [
          "on",
          "off"
        ]
      }
    },
    {
      "name": "timeout",
      "title": "Timeout",
      "defaultValue": 10000,
      "type": "duration"
    },
    {
      "name": "accelerationMode",
      "title": "Acceleration Mode",
      "defaultValue": "auto",
      "type": {
        "select": [
          "auto",
          "on",
          "off"
        ]
      }
    },
    {
      "name": "attackLimit",
      "title": "Attack Limit",
      "defaultValue": 1500000000,
      "type": "integer"
    },
    {
      "name": "attackTimeout",
      "title": "Attack Timeout",
      "defaultValue": 1000,
      "type": "duration"
    },
    {
      "name": "crossoverSize",
      "title": "Crossover Size",
      "defaultValue": 25,
      "type": "integer"
    },
    {
      "name": "heatRatio",
      "title": "Heat Ratio",
      "defaultValue": 0.001,
      "type": "float"
    },
    {
      "name": "incubationLimit",
      "title": "Incubation Limit",
      "defaultValue": 25000,
      "type": "integer"
    },
    {
      "name": "incubationTimeout",
      "title": "Incubation Timeout",
      "defaultValue": 250,
      "type": "duration"
    },
    {
      "name": "maxAttackStringSize",
      "title": "Max Attack String Size",
      "defaultValue": 300000,
      "type": "integer"
    },
    {
      "name": "maxDegree",
      "title": "Max Degree",
      "defaultValue": 4,
      "type": "integer"
    },
    {
      "name": "maxGeneStringSize",
      "title": "Max Gene String Size",
      "defaultValue": 2400,
      "type": "integer"
    },
    {
      "name": "maxGenerationSize",
      "title": "Max Generation Size",
      "defaultValue": 100,
      "type": "integer"
    },
    {
      "name": "maxInitialGenerationSize",
      "title": "Max Initial Generation Size",
      "defaultValue": 500,
      "type": "integer"
    },
    {
      "name": "maxIteration",
      "title": "Max Iteration",
      "defaultValue": 10,
      "type": "integer"
    },
    {
      "name": "maxNFASize",
      "title": "Max NFA Size",
      "defaultValue": 35000,
      "type": "integer"
    },
    {
      "name": "maxPatternSize",
      "title": "Max Pattern Size",
      "defaultValue": 1500,
      "type": "integer"
    },
    {
      "name": "maxRecallStringSize",
      "title": "Max Recall String Size",
      "defaultValue": 300000,
      "type": "integer"
    },
    {
      "name": "maxRepeatCount",
      "title": "Max Repeat Count",
      "defaultValue": 30,
      "type": "integer"
    },
    {
      "name": "maxSimpleRepeatCount",
      "title": "Max Simple Repeat Count",
      "defaultValue": 30,
      "type": "integer"
    },
    {
      "name": "mutationSize",
      "title": "Mutation Size",
      "defaultValue": 50,
      "type": "integer"
    },
    {
      "name": "randomSeed",
      "title": "Random Seed",
      "defaultValue": 0,
      "type": "integer"
    },
    {
      "name": "recallLimit",
      "title": "Recall Limit",
      "defaultValue": 1500000000,
      "type": "integer"
    },
    {
      "name": "recallTimeout",
      "title": "Recall Timeout",
      "defaultValue": -1000,
      "type": "duration"
    },
    {
      "name": "seeder",
      "title": "Seeder",
      "defaultValue": "static",
      "type": {
        "select": [
          "static",
          "dynamic"
        ]
      }
    },
    {
      "name": "seedingLimit",
      "title": "Seeding Limit",
      "defaultValue": 1000,
      "type": "integer"
    },
    {
      "name": "seedingTimeout",
      "title": "Seeding Timeout",
      "defaultValue": 100,
      "type": "duration"
    }
  ];

  return (
    <div className="card margin-vert--lg">
      <div className="card__header"><h3>RegExp</h3></div>
      <div className="card__body">
        <div className="container">
          <div className="row row--no-gutters margin-top-sm">
            <div className="col col--12">
              <TextArea name="input" defaultValue="/^(a|a)*$/" placeholder="/^(a|a)*$/" />
            </div>
          </div>

          <div className="row margin-top--sm row--no-gutters">
            <div className="col col--12 text--right">
              {
                status !== 'running' ? null :
                  <button className="button button--secondary" onClick={() => cancel()}>Cancel</button>
              }
              <button className="button button--primary" onClick={() => check()} disabled={status === 'running'}>Check</button>
            </div>
          </div>

          <div className="row margin-top--sm row--no-gutters">
            <div className="col col--12">
              <DemoParams params={params} />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

type DemoResultProps = {
  status: "init" | "running" | "done";
  log: string[];
  diagnostics: Diagnostics | InvalidDiagnostics | null;
  time: number;
};

const DemoResult: React.VFC<DemoResultProps> = ({ status, log, diagnostics, time }) => {
  const logBottomRef = React.useRef<HTMLDivElement>();
  React.useEffect(() => {
    logBottomRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [log]);

  if (!diagnostics) {
    if (status === 'init') {
      return null;
    }

    return (
      <div className="card margin-vert--lg">
        <div className="card__header"><h3>Result</h3></div>
        <div className="card__body">
          <div className="container">
            <div className="row">
              <div className="col col--12 text--center">
                <ReactLoading type="bars" width={128} color={'var(--ifm-font-color-base)'} className={styles.demoResultLoading} />
              </div>
            </div>
            {log.length > 0 ? (
              <div className="row">
                <div className="col col--3"><h4>Log</h4></div>
                <div className="col col--9">
                  <pre className={styles.demoResultLog}>
                    {log.join('\n')}
                    <div ref={logBottomRef} />
                  </pre>
                </div>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    );
  }

  if (diagnostics.status === 'invalid') {
    return (
      <div className="card margin-vert--lg">
        <div className="card margin-vert--lg">
          <div className="card__header"><h3>Result</h3></div>
          <div className="card__body">
            <div className="container">
              <div className="row">
                <div className="col col--3"><h4>Input</h4></div>
                <div className="col col--9"><code>{diagnostics.input}</code></div>
              </div>
              <div className="row">
                <div className="col col--3"><h4>Time</h4></div>
                <div className="col col--9"><p>{time.toFixed(2)} s</p></div>
              </div>
              <div className="row">
                <div className="col col--3"><h4>Status</h4></div>
                <div className="col col--9"><p><span className="badge badge--secondary">invalid</span></p></div>
              </div>
              {log.length > 0 ? (
                <div className="row">
                  <div className="col col--3"><h4>Log</h4></div>
                  <div className="col col--9">
                    <pre className={styles.demoResultLog}>{log.join('\n')}</pre>
                  </div>
                </div>
              ) : null}
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (diagnostics.status === 'unknown') {
    return (
      <div className="card margin-vert--lg">
        <div className="card margin-vert--lg">
          <div className="card__header"><h3>Result</h3></div>
          <div className="card__body">
            <div className="container">
              <div className="row">
                <div className="col col--3"><h4>Input</h4></div>
                <div className="col col--9"><pre>{`/${diagnostics.source}/${diagnostics.flags}`}</pre></div>
              </div>
              <div className="row">
                <div className="col col--3"><h4>Time</h4></div>
                <div className="col col--9"><p>{time.toFixed(2)} s</p></div>
              </div>
              <div className="row">
                <div className="col col--3"><h4>Status</h4></div>
                <div className="col col--9"><p><span className="badge badge--secondary">unknown</span></p></div>
              </div>
              <div className="row">
                <div className="col col--3"><h4>Error Kind</h4></div>
                <div className="col col--9"><p>{diagnostics.error.kind}</p></div>
              </div>
              {
                diagnostics.error.kind === 'timeout' || diagnostics.error.kind === 'cancel' ? null : (
                  <div className="row">
                    <div className="col col--3"><h4>Error Message</h4></div>
                    <div className="col col--9"><p>{diagnostics.error.message}</p></div>
                  </div>
                )
              }
              {
                diagnostics.checker ? (
                  <div className="row">
                    <div className="col col--3"><h4>Checker</h4></div>
                    <div className="col col--9">
                      <p><span className="badge badge--secondary">{diagnostics.checker}</span></p>
                    </div>
                  </div>
                ) : null
              }
              {log.length > 0 ? (
                <div className="row">
                  <div className="col col--3"><h4>Log</h4></div>
                  <div className="col col--9">
                    <pre className={styles.demoResultLog}>{log.join('\n')}</pre>
                  </div>
                </div>
              ) : null}
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (diagnostics.status === 'safe') {
    return (
      <div className="card margin-vert--lg">
        <div className="card__header"><h3>Result</h3></div>
        <div className="card__body">
          <div className="container">
            <div className="row">
              <div className="col col--3"><h4>Input</h4></div>
              <div className="col col--9"><pre>{`/${diagnostics.source}/${diagnostics.flags}`}</pre></div>
            </div>
            <div className="row">
              <div className="col col--3"><h4>Time</h4></div>
              <div className="col col--9"><p>{time.toFixed(2)} s</p></div>
            </div>
            <div className="row">
              <div className="col col--3"><h4>Status</h4></div>
              <div className="col col--9"><p><span className="badge badge--info">safe</span></p></div>
            </div>
            <div className="row">
              <div className="col col--3"><h4>Checker</h4></div>
              <div className="col col--9">
                <p><span className="badge badge--secondary">{diagnostics.checker}</span></p>
              </div>
            </div>
            {log.length > 0 ? (
              <div className="row">
                <div className="col col--3"><h4>Log</h4></div>
                <div className="col col--9">
                  <pre className={styles.demoResultLog}>{log.join('\n')}</pre>
                </div>
              </div>
            ) : null}
          </div>
        </div>
      </div>
    );
  }

  let index = 0;
  const spots = [];
  for (const { start, end, temperature } of diagnostics.hotspot) {
    if (index < start) {
      spots.push(<span key={index}>{diagnostics.source.substring(index, start)}</span>);
    }
    const className = temperature === 'heat' ? styles.demoResultHotspotHeat : styles.demoResultHotspotNormal;
    spots.push(<span key={start} className={className}>{diagnostics.source.substring(start, end)}</span>);
    index = end;
  }
  if (index < diagnostics.source.length) {
    spots.push(<span key={index}>{diagnostics.source.substring(index)}</span>);
  }

  const badge = diagnostics.complexity.type === 'exponential' ? 'danger' : 'warning';

  return (
    <div className="card margin-vert--lg">
      <div className="card__header"><h3>Result</h3></div>
      <div className="card__body">
        <div className="container">
          <div className="row">
            <div className="col col--3"><h4>Input</h4></div>
            <div className="col col--9"><pre>{`/${diagnostics.source}/${diagnostics.flags}`}</pre></div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Time</h4></div>
            <div className="col col--9"><p>{time.toFixed(2)} s</p></div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Status</h4></div>
            <div className="col col--9"><p><span className="badge badge--danger">vulnerable</span></p></div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Complexity</h4></div>
            <div className="col col--9">
              <p><span className={`badge badge--${badge}`}>{diagnostics.complexity.summary}</span></p>
            </div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Attack String</h4></div>
            <div className="col col--9"><pre><code>{diagnostics.attack.pattern}</code></pre></div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Hotspot</h4></div>
            <div className="col col--9"><pre><code>/{spots}/</code></pre></div>
          </div>
          <div className="row">
            <div className="col col--3"><h4>Checker</h4></div>
            <div className="col col--9">
              <p><span className="badge badge--secondary">{diagnostics.checker}</span></p>
            </div>
          </div>
          {log.length > 0 ? (
            <div className="row">
              <div className="col col--3"><h4>Log</h4></div>
              <div className="col col--9">
                <pre className={styles.demoResultLog}>{log.join('\n')}</pre>
              </div>
            </div>
          ) : null}
        </div>
      </div>
    </div>
  );
};

const extract = (input: string): { source: string; flags: string } | null => {
  if (!input.startsWith("/")) return null;

  const lastSlashPos = input.lastIndexOf('/');
  if (lastSlashPos === 0) return null;

  return {
    source: input.slice(1, lastSlashPos),
    flags: input.slice(lastSlashPos + 1),
  };
};

type InvalidDiagnostics = { input: string; status: "invalid" };

const Demo: React.VFC<{}> = () => {
  const [status, setStatus] = React.useState<'init' | 'running' | 'done'>('init');
  const [diagnostics, setDiagnostics] = React.useState<Diagnostics | InvalidDiagnostics | null>(null);
  const [log, setLog] = React.useState<string[]>([]);
  const [time, setTime] = React.useState<number>(0);
  const abortControllerRef = React.useRef<AbortController>();

  const methods = useForm();

  const check = methods.handleSubmit(async (params) => {
    setStatus('running');
    setDiagnostics(null);
    setLog([]);
    setTime(0);
    abortControllerRef.current = null;

    const input = params.input;
    delete params.input;

    const extracted = extract(input);
    if (extracted === null) {
      setStatus('done');
      setDiagnostics({ input, status: 'invalid' });
    }
    const { source, flags } = extracted;

    if (params.logger === 'on') {
      params.logger = (message: string) => {
        console.log(message);
        setLog(log => log.concat(message));
      };
    } else {
      params.logger = null;
    }

    const controller = new AbortController();
    abortControllerRef.current = controller;
    params.signal = controller.signal;

    const start = Date.now();
    const diagnostics = await recheck.check(source, flags, params);
    const time = (Date.now() - start) / 1000;

    setStatus('done');
    setDiagnostics(diagnostics);
    setTime(time);
    abortControllerRef.current = null;
  });

  const cancel = React.useCallback(() => {
    abortControllerRef.current?.abort();
  }, [abortControllerRef]);

  return (
    <FormProvider {...methods}>
      <div className="container">
        <DemoInput status={status} check={check} cancel={cancel} />
        <DemoResult status={status} log={log} diagnostics={diagnostics} time={time} />
      </div>
    </FormProvider>
  );
};

export default Demo;
