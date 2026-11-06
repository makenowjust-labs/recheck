import React from 'react';

import styles from './Hotspot.module.css';

import type { Hotspot as HotspotSpot } from 'recheck';

/** Shows the source of a pattern with its hotspot highlighted. */
const Hotspot: React.FC<{ source: string; hotspot: readonly HotspotSpot[] }> = ({ source, hotspot }) => {
  const parts = [];
  let index = 0;
  for (const { start, end, temperature } of hotspot) {
    if (index < start) {
      parts.push(<span key={index}>{source.substring(index, start)}</span>);
    }
    const className = temperature === 'hot' ? styles.hotspotHot : styles.hotspotWarm;
    parts.push(
      <span key={start} className={className}>
        {source.substring(start, end)}
      </span>,
    );
    index = end;
  }
  if (index < source.length) {
    parts.push(<span key={index}>{source.substring(index)}</span>);
  }
  return (
    <pre>
      <code>/{parts}/</code>
    </pre>
  );
};

export default Hotspot;
