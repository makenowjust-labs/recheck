import clsx from 'clsx';
import React from 'react';
import { useController, useFormContext } from 'react-hook-form';

import styles from './NumberInput.module.css';

export type Props = React.InputHTMLAttributes<HTMLInputElement>;

const NumberInput: React.VFC<Props> = ({ className, name, defaultValue, ...props }) => {
  const { control } = useFormContext();
  const { field } = useController({
    control,
    name,
    defaultValue,
  });

  return (
    <input type="number" className={clsx(styles.numberInput, className)} {...props} {...field} />
  );
};

export default NumberInput;
