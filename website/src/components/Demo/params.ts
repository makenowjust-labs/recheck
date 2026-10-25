import type { Parameters } from 'recheck';

export type ParamType = 'duration' | 'float' | 'integer' | { select: string[] };

export type Param = {
  title: string;
  name: string;
  defaultValue: string | number;
  type: ParamType;
};

export const params: Param[] = [
  {
    name: 'checker',
    title: 'Checker',
    defaultValue: 'auto',
    type: { select: ['auto', 'automaton', 'fuzz'] },
  },
  {
    name: 'logger',
    title: 'Logger',
    defaultValue: 'off',
    type: { select: ['on', 'off'] },
  },
  {
    name: 'timeout',
    title: 'Timeout',
    defaultValue: 10000,
    type: 'duration',
  },
  {
    name: 'accelerationMode',
    title: 'Acceleration Mode',
    defaultValue: 'auto',
    type: { select: ['auto', 'on', 'off'] },
  },
  {
    name: 'attackLimit',
    title: 'Attack Limit',
    defaultValue: 1500000000,
    type: 'integer',
  },
  {
    name: 'attackTimeout',
    title: 'Attack Timeout',
    defaultValue: 1000,
    type: 'duration',
  },
  {
    name: 'crossoverSize',
    title: 'Crossover Size',
    defaultValue: 25,
    type: 'integer',
  },
  {
    name: 'heatRatio',
    title: 'Heat Ratio',
    defaultValue: 0.001,
    type: 'float',
  },
  {
    name: 'incubationLimit',
    title: 'Incubation Limit',
    defaultValue: 25000,
    type: 'integer',
  },
  {
    name: 'incubationTimeout',
    title: 'Incubation Timeout',
    defaultValue: 250,
    type: 'duration',
  },
  {
    name: 'maxAttackStringSize',
    title: 'Max Attack String Size',
    defaultValue: 300000,
    type: 'integer',
  },
  {
    name: 'maxDegree',
    title: 'Max Degree',
    defaultValue: 4,
    type: 'integer',
  },
  {
    name: 'maxGeneStringSize',
    title: 'Max Gene String Size',
    defaultValue: 2400,
    type: 'integer',
  },
  {
    name: 'maxGenerationSize',
    title: 'Max Generation Size',
    defaultValue: 100,
    type: 'integer',
  },
  {
    name: 'maxInitialGenerationSize',
    title: 'Max Initial Generation Size',
    defaultValue: 500,
    type: 'integer',
  },
  {
    name: 'maxIteration',
    title: 'Max Iteration',
    defaultValue: 10,
    type: 'integer',
  },
  {
    name: 'maxNFASize',
    title: 'Max NFA Size',
    defaultValue: 35000,
    type: 'integer',
  },
  {
    name: 'maxPatternSize',
    title: 'Max Pattern Size',
    defaultValue: 1500,
    type: 'integer',
  },
  {
    name: 'maxRecallStringSize',
    title: 'Max Recall String Size',
    defaultValue: 300000,
    type: 'integer',
  },
  {
    name: 'maxRepeatCount',
    title: 'Max Repeat Count',
    defaultValue: 30,
    type: 'integer',
  },
  {
    name: 'maxSimpleRepeatCount',
    title: 'Max Simple Repeat Count',
    defaultValue: 30,
    type: 'integer',
  },
  {
    name: 'mutationSize',
    title: 'Mutation Size',
    defaultValue: 50,
    type: 'integer',
  },
  {
    name: 'randomSeed',
    title: 'Random Seed',
    defaultValue: 0,
    type: 'integer',
  },
  {
    name: 'recallLimit',
    title: 'Recall Limit',
    defaultValue: 1500000000,
    type: 'integer',
  },
  {
    name: 'recallTimeout',
    title: 'Recall Timeout',
    defaultValue: -1000,
    type: 'duration',
  },
  {
    name: 'seeder',
    title: 'Seeder',
    defaultValue: 'static',
    type: { select: ['static', 'dynamic'] },
  },
  {
    name: 'seedingLimit',
    title: 'Seeding Limit',
    defaultValue: 1000,
    type: 'integer',
  },
  {
    name: 'seedingTimeout',
    title: 'Seeding Timeout',
    defaultValue: 100,
    type: 'duration',
  },
];

/** `logger` is a Playground option rather than a `recheck` parameter. */
export type CheckParams = Parameters & { logger: 'on' | 'off' };

/** Converts the form values, where numbers are strings, to the parameters. */
export const parseParams = (values: Record<string, unknown>): CheckParams => {
  const parsed: Record<string, unknown> = {};
  for (const { name, type, defaultValue } of params) {
    const value = values[name] ?? defaultValue;
    switch (type) {
      case 'duration':
      case 'integer':
        parsed[name] = Number.parseInt(String(value), 10);
        break;
      case 'float':
        parsed[name] = Number.parseFloat(String(value));
        break;
      default:
        parsed[name] = String(value);
    }
  }
  return parsed as CheckParams;
};

export const extract = (input: string): { source: string; flags: string } | null => {
  if (!input.startsWith('/')) return null;

  const lastSlashPos = input.lastIndexOf('/');
  if (lastSlashPos === 0) return null;

  return {
    source: input.slice(1, lastSlashPos),
    flags: input.slice(lastSlashPos + 1),
  };
};
