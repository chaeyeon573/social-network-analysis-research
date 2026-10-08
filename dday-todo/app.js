(function () {
  'use strict';

  const DDAY_KEY = 'ddayTodo.ddays';
  const TODO_KEY = 'ddayTodo.todos';
  const TAB_KEY = 'ddayTodo.tab';

  const TRASH_SVG =
    '<svg viewBox="0 0 24 24" aria-hidden="true"><path d="M9 3v1H4v2h16V4h-5V3H9zM6 8l1 13h10l1-13H6z"/></svg>';

  // ---------- 저장소 ----------
  function load(key, fallback) {
    try {
      const raw = localStorage.getItem(key);
      return raw ? JSON.parse(raw) : fallback;
    } catch (e) {
      return fallback;
    }
  }
  function save(key, value) {
    try {
      localStorage.setItem(key, JSON.stringify(value));
    } catch (e) {
      /* 저장 공간 부족 등은 무시 */
    }
  }
  function uid() {
    return Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
  }

  let ddays = load(DDAY_KEY, []);
  let todos = load(TODO_KEY, []);

  // ---------- 날짜 ----------
  // 'YYYY-MM-DD'를 로컬 자정 기준 Date로 변환
  function parseDate(str) {
    const [y, m, d] = str.split('-').map(Number);
    return new Date(y, m - 1, d);
  }
  function todayMidnight() {
    const now = new Date();
    return new Date(now.getFullYear(), now.getMonth(), now.getDate());
  }
  function daysUntil(str) {
    // 서머타임 등 시차를 피하기 위해 UTC 기준으로 일수 계산
    const t = parseDate(str);
    const n = todayMidnight();
    const a = Date.UTC(t.getFullYear(), t.getMonth(), t.getDate());
    const b = Date.UTC(n.getFullYear(), n.getMonth(), n.getDate());
    return Math.round((a - b) / 86400000);
  }
  function ddayLabel(diff) {
    if (diff === 0) return 'D-Day';
    return diff > 0 ? 'D-' + diff : 'D+' + Math.abs(diff);
  }
  const WEEKDAYS = ['일', '월', '화', '수', '목', '금', '토'];
  function formatDate(d) {
    return d.getFullYear() + '.' + String(d.getMonth() + 1).padStart(2, '0') + '.' +
      String(d.getDate()).padStart(2, '0') + ' (' + WEEKDAYS[d.getDay()] + ')';
  }
  function toInputValue(d) {
    return d.getFullYear() + '-' + String(d.getMonth() + 1).padStart(2, '0') + '-' +
      String(d.getDate()).padStart(2, '0');
  }

  const $ = (id) => document.getElementById(id);

  // ---------- 디데이 ----------
  function renderDdays() {
    const list = $('dday-list');
    list.innerHTML = '';

    // 다가오는 이벤트(가까운 순) → 지난 이벤트(최근 순)
    const items = ddays
      .map((e) => ({ ...e, diff: daysUntil(e.date) }))
      .sort((a, b) => {
        const ap = a.diff < 0, bp = b.diff < 0;
        if (ap !== bp) return ap ? 1 : -1;
        return ap ? b.diff - a.diff : a.diff - b.diff;
      });

    for (const e of items) {
      const li = document.createElement('li');
      li.className = 'item';

      const badge = document.createElement('div');
      badge.className = 'badge' + (e.diff === 0 ? ' today' : e.diff < 0 ? ' past' : '');
      badge.textContent = ddayLabel(e.diff);

      const body = document.createElement('div');
      body.className = 'item-body';
      const title = document.createElement('div');
      title.className = 'item-title';
      title.textContent = e.name;
      const sub = document.createElement('div');
      sub.className = 'item-sub';
      sub.textContent = formatDate(parseDate(e.date)) +
        (e.diff > 0 ? ' · ' + e.diff + '일 남음' : e.diff < 0 ? ' · ' + -e.diff + '일 지남' : ' · 오늘');
      body.append(title, sub);

      const del = document.createElement('button');
      del.type = 'button';
      del.className = 'icon-btn';
      del.setAttribute('aria-label', e.name + ' 삭제');
      del.innerHTML = TRASH_SVG;
      del.addEventListener('click', () => {
        if (!confirm('"' + e.name + '" 디데이를 삭제할까요?')) return;
        ddays = ddays.filter((x) => x.id !== e.id);
        save(DDAY_KEY, ddays);
        renderDdays();
      });

      li.append(badge, body, del);
      list.appendChild(li);
    }
    $('dday-empty').hidden = items.length > 0;
  }

  $('dday-form').addEventListener('submit', (ev) => {
    ev.preventDefault();
    const name = $('dday-name').value.trim();
    const date = $('dday-date').value;
    if (!name || !date) return;
    ddays.push({ id: uid(), name, date });
    save(DDAY_KEY, ddays);
    $('dday-name').value = '';
    $('dday-name').focus();
    renderDdays();
  });

  // ---------- 투두 ----------
  function todoItem(t) {
    const li = document.createElement('li');
    li.className = 'item' + (t.done ? ' done' : '');

    const check = document.createElement('input');
    check.type = 'checkbox';
    check.className = 'check';
    check.checked = t.done;
    check.setAttribute('aria-label', t.text + (t.done ? ' 완료 취소' : ' 완료'));
    check.addEventListener('change', () => {
      t.done = check.checked;
      t.doneAt = t.done ? Date.now() : null;
      save(TODO_KEY, todos);
      renderTodos();
    });

    const body = document.createElement('div');
    body.className = 'item-body';
    const title = document.createElement('div');
    title.className = 'item-title';
    title.textContent = t.text;
    body.appendChild(title);
    body.addEventListener('click', () => check.click());

    const del = document.createElement('button');
    del.type = 'button';
    del.className = 'icon-btn';
    del.setAttribute('aria-label', t.text + ' 삭제');
    del.innerHTML = TRASH_SVG;
    del.addEventListener('click', () => {
      todos = todos.filter((x) => x.id !== t.id);
      save(TODO_KEY, todos);
      renderTodos();
    });

    li.append(check, body, del);
    return li;
  }

  function renderTodos() {
    const active = todos.filter((t) => !t.done).sort((a, b) => a.createdAt - b.createdAt);
    const done = todos.filter((t) => t.done).sort((a, b) => (b.doneAt || 0) - (a.doneAt || 0));

    const activeList = $('todo-active');
    const doneList = $('todo-done');
    activeList.innerHTML = '';
    doneList.innerHTML = '';
    active.forEach((t) => activeList.appendChild(todoItem(t)));
    done.forEach((t) => doneList.appendChild(todoItem(t)));

    $('todo-active-count').textContent = active.length;
    $('todo-done-count').textContent = done.length;
    $('todo-active-empty').hidden = active.length > 0;
    $('todo-done-empty').hidden = done.length > 0;
    $('todo-clear-done').hidden = done.length === 0;
  }

  $('todo-form').addEventListener('submit', (ev) => {
    ev.preventDefault();
    const text = $('todo-text').value.trim();
    if (!text) return;
    todos.push({ id: uid(), text, done: false, createdAt: Date.now(), doneAt: null });
    save(TODO_KEY, todos);
    $('todo-text').value = '';
    $('todo-text').focus();
    renderTodos();
  });

  $('todo-clear-done').addEventListener('click', () => {
    if (!confirm('완료한 항목을 모두 삭제할까요?')) return;
    todos = todos.filter((t) => !t.done);
    save(TODO_KEY, todos);
    renderTodos();
  });

  // ---------- 탭 ----------
  function selectTab(name) {
    document.querySelectorAll('.tabbar [role="tab"]').forEach((btn) => {
      const on = btn.dataset.tab === name;
      btn.setAttribute('aria-selected', String(on));
      $('panel-' + btn.dataset.tab).hidden = !on;
    });
    save(TAB_KEY, name);
  }
  document.querySelectorAll('.tabbar [role="tab"]').forEach((btn) => {
    btn.addEventListener('click', () => selectTab(btn.dataset.tab));
  });

  // ---------- 초기화 ----------
  function renderToday() {
    $('today').textContent = '오늘 ' + formatDate(todayMidnight());
  }

  $('dday-date').value = toInputValue(todayMidnight());
  selectTab(load(TAB_KEY, 'dday') === 'todo' ? 'todo' : 'dday');
  renderToday();
  renderDdays();
  renderTodos();

  // 앱을 켜둔 채 날짜가 바뀌거나 다시 열었을 때 디데이 갱신
  document.addEventListener('visibilitychange', () => {
    if (document.visibilityState === 'visible') {
      renderToday();
      renderDdays();
    }
  });

  // 다른 탭에서 변경된 데이터 반영
  window.addEventListener('storage', (ev) => {
    if (ev.key === DDAY_KEY) { ddays = load(DDAY_KEY, []); renderDdays(); }
    if (ev.key === TODO_KEY) { todos = load(TODO_KEY, []); renderTodos(); }
  });

  // ---------- 서비스 워커 ----------
  if ('serviceWorker' in navigator) {
    window.addEventListener('load', () => {
      navigator.serviceWorker.register('sw.js').catch(() => {});
    });
  }
})();
