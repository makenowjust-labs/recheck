import Link from '@docusaurus/Link';
import Heading from '@theme/Heading';
import React from 'react';

import { parseMarkdown } from './markdown';
import { sections } from './schema';

import type { Inline } from './markdown';
import type { Section } from './schema';

export { toc } from './schema';

const Inlines: React.FC<{ inlines: Inline[] }> = ({ inlines }) => (
  <>
    {inlines.map((inline, index) => {
      switch (inline.type) {
        case 'text':
          return <React.Fragment key={index}>{inline.value}</React.Fragment>;
        case 'code':
          return <code key={index}>{inline.value}</code>;
        case 'link':
          return (
            <Link key={index} href={inline.href}>
              <Inlines inlines={inline.children} />
            </Link>
          );
      }
    })}
  </>
);

const Description: React.FC<{ markdown: string }> = ({ markdown }) => (
  <>
    {parseMarkdown(markdown).map((block, index) =>
      block.type === 'paragraph' ? (
        <p key={index}>
          <Inlines inlines={block.children} />
        </p>
      ) : (
        <ul key={index}>
          {block.items.map((item, index) => (
            <li key={index}>
              <Inlines inlines={item} />
            </li>
          ))}
        </ul>
      ),
    )}
  </>
);

const ParameterSection: React.FC<{ section: Section }> = ({ section }) => (
  <>
    <Heading as="h2" id={section.id}>
      <code>{section.name}</code>
    </Heading>
    <ul>
      <li>
        Type: <code>{section.typescriptType}</code> (TypeScript
        {section.unit === 'milliseconds' ? ', in milliseconds' : ''}), <code>{section.scalaType}</code> (Scala)
      </li>
      <li>
        Default: <code>{section.typescriptDefault}</code>
        {section.typescriptDefault === section.scalaDefault ? null : (
          <>
            {' '}
            (TypeScript), <code>{section.scalaDefault}</code> (Scala)
          </>
        )}
      </li>
      {section.cli === null ? null : (
        <li>
          CLI: <code>--{section.cli}</code>
        </li>
      )}
      {section.since === null ? null : <li>Since: v{section.since}</li>}
    </ul>
    <Description markdown={section.description} />
  </>
);

/** Parameters renders a section of each parameter of the schema, whose headings are in the exported `toc`. */
const Parameters: React.FC = () => (
  <>
    {sections.map((section) => (
      <ParameterSection key={section.name} section={section} />
    ))}
  </>
);

export default Parameters;
