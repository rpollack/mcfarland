import ReactMarkdown from "react-markdown";

interface Props {
  children: string;
}

// Kept in its own module so callers can lazy-load react-markdown out of the main bundle.
function Markdown({ children }: Props) {
  return <ReactMarkdown>{children}</ReactMarkdown>;
}

export default Markdown;
