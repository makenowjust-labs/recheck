import clsx from 'clsx';
import React from 'react';

import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import useThemeContext from '@theme/hooks/useThemeContext';

import styles from './index.module.css';
import HomepageFeatures from '../components/HomepageFeatures';
import LogoLight from '../../static/img/logo-light.svg';
import LogoDark from '../../static/img/logo-dark.svg';

function HomeHeader() {
  const { siteConfig } = useDocusaurusContext();
  const { isDarkTheme } = useThemeContext();
  const Logo = isDarkTheme ? LogoDark : LogoLight;
  return (
    <header className={clsx('hero', styles.heroBanner)}>
      <div className="container">
        <Logo className={styles.heroBannerLogo} />
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
