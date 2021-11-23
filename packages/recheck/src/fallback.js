import * as bundle from '../../../modules/recheck-js/target/scala-2.13/recheck-js-opt/recheck';

export async function check(source, flags, config = {}) {
  return await new Promise((resolve) => resolve(bundle.check(source, flags, config)));
}
