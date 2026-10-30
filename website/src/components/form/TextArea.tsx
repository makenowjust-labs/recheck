import clsx from 'clsx';
import React from 'react';
import { useController, useFormContext } from 'react-hook-form';

import styles from './TextArea.module.css';

export type Props = { name: string } & React.TextareaHTMLAttributes<HTMLTextAreaElement>;

const TextArea: React.FC<Props> = ({ className, name, defaultValue, ...props }) => {
  const { control } = useFormContext();
  const { field } = useController({
    control,
    name,
    defaultValue,
  });

  return (
    <textarea className={clsx(styles.textArea, className)} {...props} {...field} />
  );
};

export default TextArea;
