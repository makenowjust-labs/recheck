import clsx from 'clsx';
import React from 'react';
import { useController, useFormContext } from 'react-hook-form';

import styles from './Select.module.css';

type Props = {
  values: string[];
  defaultValue: string;
} & React.SelectHTMLAttributes<HTMLSelectElement>;

const Select: React.VFC<Props> = ({ name, values, defaultValue, className, ...props }) => {
  const { control } = useFormContext();
  const { field } = useController({
    control,
    name,
    defaultValue,
  });

  const options = values.map(value => (
    <option value={value} key={value}>{value}</option>
  ));

  return (
    <div className={clsx(styles.select, className)}>
      <select className={styles.selectSelect} {...props} {...field}>
        {options}
      </select>
    </div>
  );
};

export default Select;
