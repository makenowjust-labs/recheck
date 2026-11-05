import prismIncludeLanguages from "@theme-original/prism-include-languages";

export default function prismIncludeLanguagesWithConsole(PrismObject) {
  prismIncludeLanguages(PrismObject);
  PrismObject.languages.console = PrismObject.languages["shell-session"];
}
