(() => {
  'use strict';

  const initialize = () => {
    const SECOND = 1000;
    const MINUTE = 60 * SECOND;
    const HOUR = 60 * MINUTE;
    const DAY = 24 * HOUR;
    const WEEK = 7 * DAY;
    const MONTH = 4 * WEEK;
    const YEAR = 13 * MONTH;

    const RTF = new Intl.RelativeTimeFormat();

    const fromNow = (date, now, rtf) => {
      const delta = date - now;
      const duration = Math.abs(delta);

      if (duration < MINUTE) { return rtf.format(Math.round(delta / SECOND), 'second'); }
      if (duration < HOUR) { return rtf.format(Math.round(delta / MINUTE), 'minute'); }
      if (duration < DAY) { return rtf.format(Math.round(delta / HOUR), 'hour'); }
      if (duration < WEEK) { return rtf.format(Math.round(delta / DAY), 'day'); }
      if (duration < MONTH) { return rtf.format(Math.round(delta / WEEK), 'week'); }
      if (duration < YEAR) { return rtf.format(Math.round(delta / MONTH), 'month'); }

      return rtf.format(Math.round(delta / YEAR), 'year');
    };

    const nextDelay = (date, now) => {
      const duration = Math.abs(date - now);

      if (duration < MINUTE) { return SECOND; }
      if (duration < HOUR) { return MINUTE; }
      if (duration < DAY) { return HOUR; }

      return DAY;
    };

    const updateContent = (element, date, now, rtf) => {
      const before = element.textContent;
      const after = fromNow(date, now, rtf);
      if (after !== before) {
        element.textContent = after;
      }
    };

    document.querySelectorAll('time.relative').forEach((element) => {
      const date = Date.parse(element.getAttribute('datetime'));

      const loop = () => {
        const now = Date.now();
        updateContent(element, date, now, RTF);
        setTimeout(loop, nextDelay(date, now));
      };
      loop();
    });
  };

  if (document.readyState !== 'loading') {
    initialize();
  } else {
    window.addEventListener('DOMContentLoaded', initialize);
  }
})();
