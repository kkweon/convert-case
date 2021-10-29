module.exports = {
  purge: {
    mode: "all",
    preserveHtmlElements: false,
    content: ["./src/**/*.elm", "./src/**/*.html"],
  },
  darkMode: false, // or 'media' or 'class'
  theme: {
    extend: {},
  },
  variants: {
    extend: {},
  },
  plugins: [],
};
