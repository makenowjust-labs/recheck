import clsx from 'clsx';
import React from 'react';

import useBaseUrl from '@docusaurus/useBaseUrl';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import ThemedImage from '@theme/ThemedImage';
import useThemeContext from '@theme/hooks/useThemeContext';

import styles from './index.module.css';
import HomepageFeatures from '../components/HomepageFeatures';
import LogoLight from '../../static/img/logo-light.svg';
import LogoDark from '../../static/img/logo-dark.svg';

function HomeHeader() {
  const { siteConfig } = useDocusaurusContext();
  const sources = {
    light: useBaseUrl('/img/logo-light.svg'),
    dark: useBaseUrl('/img/logo-dark.svg'),
  };
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <ThemedImage sources={sources} width={300} height={300} title={'recheck logo'} />
        <h1 className={clsx('hero__title', styles.heroBannerText)}>{siteConfig.title}</h1>
        <p className={clsx('hero__subtitle', styles.heroBannerText)}>{siteConfig.tagline}</p>
      </div>
    </header>
  );
}

export default function Home() {
  const { siteConfig } = useDocusaurusContext();
  return (
    <Layout
      title=""
      description={`${siteConfig.title}: ${siteConfig.tagline}`}>
      <HomeHeader />
      <main>
        <HomepageFeatures />
      </main>
    </Layout>
  );
}
