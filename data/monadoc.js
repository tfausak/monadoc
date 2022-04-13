(() => {
    const SECOND = 1000;
    const MINUTE = 60 * SECOND;
    const HOUR = 60 * MINUTE;
    const DAY = 24 * HOUR;
    const WEEK = 7 * DAY;
    const MONTH = 4 * WEEK;
    const YEAR = 13 * MONTH;

    const RTF = new Intl.RelativeTimeFormat();

    const fromNow = (date, now = Date.now(), rtf = new Intl.RelativeTimeFormat()) => {
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

    const nextDelay = (date, now = Date.now()) => {
        const duration = Math.abs(date - now);
        if (duration < MINUTE) { return SECOND; }
        if (duration < HOUR) { return MINUTE; }
        if (duration < DAY) { return HOUR; }
        return DAY;
    };

    document.querySelectorAll('time.timeago').forEach((element) => {
        const then = Date.parse(element.getAttribute('datetime'));
        const updateContent = (now) => {
            const before = element.textContent;
            const after = fromNow(then, now, RTF);
            if (after !== before) {
                element.textContent = after;
            }
        };
        const loop = () => {
            const now = Date.now();
            updateContent(now);
            setTimeout(loop, nextDelay(then, now));
        };
        loop();
    });
})();
