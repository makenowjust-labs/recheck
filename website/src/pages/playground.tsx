import React from 'react';
import Layout from '@theme/Layout';

import Demo from '../components/Demo';

function PlaygroundHeader() {
  return (
    <header className="hero hero--dark">
      <div className="container">
        <h1 className="hero__title">Playground</h1>
        <p className="hero__subtitle">Try recheck in your browser!</p>
      </div>
    </header>
  );
}

export default function Playground() {
  return (
    <Layout
      title="Playground"
      description={`Try recheck in your browser!`}>
      <PlaygroundHeader />
      <main>
        <Demo />
      </main>
    </Layout>
  );
}
