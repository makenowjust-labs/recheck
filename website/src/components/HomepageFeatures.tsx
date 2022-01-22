import React from 'react';
import clsx from 'clsx';

import styles from './HomepageFeatures.module.css';

const FeatureList = [
  {
    title: 'Practical Regular Expressions',
    Svg: require('../../static/img/fa-gears.svg').default,
    svgClass: styles.featureSvgGears,
    description: (
      <>
        <code>recheck</code> supports practical regular expression features including backreferences and look-around operations.
      </>
    ),
  },
  {
    title: 'State-of-the-Art',
    Svg: require('../../static/img/fa-wand-magic-sparkles.svg').default,
    svgClass: styles.featureSvgMagicWandSparkles,
    description: (
      <>
        <code>recheck</code> implements the state-of-the-art algorithm to detect ReDoS vulnerability,
        which contains the fuzzing with static analysis.
      </>
    ),
  },
  {
    title: 'Just a Library',
    Svg: require('../../static/img/fa-book-open.svg').default,
    svgClass: styles.featureSvgBookOpen,
    description: (
      <>
        <code>recheck</code> is published as just a library, so you can embed this into your application easily.
      </>
    ),
  },
];

function Feature({ Svg, title, svgClass, description }) {
  return (
    <div className={clsx('col col--4')}>
      <div className="text--center">
        <Svg className={clsx(styles.featureSvg, svgClass)} alt={title} />
      </div>
      <div className="text--center padding-horiz--md">
        <h3>{title}</h3>
        <p>{description}</p>
      </div>
    </div>
  );
}

export default function HomepageFeatures() {
  return (
    <section className={styles.features}>
      <div className="container">
        <div className="row">
          {FeatureList.map((props, idx) => (
            <Feature key={idx} {...props} />
          ))}
        </div>
      </div>
    </section>
  );
}
