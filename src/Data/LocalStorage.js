export const getItemImpl = just => nothing => key => () => {
  const res = localStorage.getItem(key);
  return res === null ? nothing : just(res);
};

export const setItem = k => v => () => localStorage.setItem(k, v);
