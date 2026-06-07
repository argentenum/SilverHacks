#!/usr/bin/env python3
import re
import csv
import time
import requests
from bs4 import BeautifulSoup
#import google.colab.files

BASE_URL = "https://www.pandulipipatala.nic.in/advance-search"
HEADERS = {"User-Agent": "Mozilla/5.0"}
TIMEOUT = 30
DELAY = 2.0 # Increased delay to 2.0 seconds

def clean_text(x):
    return re.sub(r'\s+', ' ', x or '').strip()

def find_form_inputs(soup):
    form = soup.find('form', id='frmlist') or soup.find('form', action=BASE_URL)
    if not form:
        raise RuntimeError('Search form not found')

    data = {}
    for inp in form.find_all(['input', 'select', 'textarea']):
        name = inp.get('name')
        if not name:
            continue

        if inp.name == 'input':
            t = (inp.get('type') or '').lower()
            if t in ('hidden', 'text', 'search', 'number'):
                data[name] = inp.get('value', '')
        elif inp.name == 'select':
            opts = inp.find_all('option')
            sel = None
            for o in opts:
                if o.has_attr('selected'):
                    sel = o.get('value', '')
                    break
            if sel is None and opts:
                sel = opts[0].get('value', '')
            data[name] = sel or ''
        else:
            data[name] = inp.get_text(' ', strip=True)

    return data

def extract_table_rows(html):
    soup = BeautifulSoup(html, 'html.parser')
    tables = soup.find_all('table')

    for table in tables:
        headers = [clean_text(th.get_text(' ', strip=True)) for th in table.find_all('th')]
        if headers:
            rows = []
            for tr in table.find_all('tr'):
                tds = tr.find_all('td')
                if not tds:
                    continue
                row = [clean_text(td.get_text(' ', strip=True)) for td in tds]
                rows.append(row)
            if rows:
                return headers, rows

    return None, []

def get_last_page(html):
    soup = BeautifulSoup(html, 'html.parser')
    pages = []

    for a in soup.select('.pagination a, ul.pagination a, li a'):
        txt = clean_text(a.get_text(' ', strip=True))
        if txt.isdigit():
            pages.append(int(txt))

    if pages:
        return max(pages)
    return None

def submit(session, payload):
    # Added verify=False to ignore SSL certificate verification
    r = session.post(BASE_URL, data=payload, headers=HEADERS, timeout=TIMEOUT, verify=False)
    r.raise_for_status()
    return r.text

def main():
    title_query = input('Title search text (use a single space for all results): ')
    start_page_raw = input('Start page [1]: ').strip()
    end_page_raw = input('End page [200 or detected last page]: ').strip()
    pages_per_csv_chunk_raw = input('Pages per CSV chunk [500]: ').strip() # Changed prompt
    base_filename = input('Base output CSV filename [pandulipi_results]: ').strip() or 'pandulipi_results'

    start_page = int(start_page_raw) if start_page_raw else 1
    pages_per_csv_chunk = int(pages_per_csv_chunk_raw) if pages_per_csv_chunk_raw else 500 # Renamed variable

    s = requests.Session()
    # Added verify=False to ignore SSL certificate verification
    first_response = s.get(BASE_URL, headers=HEADERS, timeout=TIMEOUT, verify=False)
    first_response.raise_for_status()

    soup = BeautifulSoup(first_response.text, 'html.parser')
    payload = find_form_inputs(soup)

    detected_last_page = get_last_page(first_response.text)
    print(f"Detected last page: {detected_last_page}")

    if end_page_raw:
        end_page = int(end_page_raw)
    elif detected_last_page:
        end_page = detected_last_page
    else:
        end_page = 200 # Default if no detection and no user input

    for k in ('squerytitle', 'querytitle', 'title', 'query'):
        if k in payload:
            payload[k] = title_query
            break
    else:
        payload['squerytitle'] = title_query

    payload['pagelimit'] = '60' # Setting to 60 to extract more rows per page
    payload['task'] = 'list'

    all_results = []
    current_chunk_start_page = start_page
    headers = None
    seen_keys = set() # Track all seen rows across chunks to avoid duplicates
    pages_in_current_chunk = 0 # New variable to track pages in current chunk

    print(f"Starting scrape from page {start_page} to {end_page}...")

    for page in range(start_page, end_page + 1):
        payload['page'] = str(page)
        html = submit(s, payload)

        page_headers, rows = extract_table_rows(html)

        if not rows:
            print(f"No rows found on page {page}; stopping.")
            break

        if page_headers and headers is None:
            headers = page_headers

        print(f"Processing page {page}: {len(rows)} rows found.")

        for row in rows:
            row_key = tuple(row)
            if row_key not in seen_keys:
                all_results.append(row)
                seen_keys.add(row_key)

        pages_in_current_chunk += 1 # Increment page count for the current chunk

        # Changed condition to use pages_in_current_chunk
        if pages_in_current_chunk >= pages_per_csv_chunk or page == end_page:
            if not all_results:
                print(f"No new results in this chunk (pages {current_chunk_start_page}-{page}). Skipping CSV generation.")
                current_chunk_start_page = page + 1
                pages_in_current_chunk = 0 # Reset page count for next chunk
                continue

            out_csv_filename = f"{base_filename}_pages_{current_chunk_start_page}_to_{page}.csv"

            width = max(len(r) for r in all_results)
            final_headers = headers or [f'col{i+1}' for i in range(width)]
            if len(final_headers) < width:
                final_headers = final_headers + [f'col{i+1}' for i in range(len(final_headers) + 1, width + 1)]

            with open(out_csv_filename, 'w', encoding='utf-8', newline='') as f:
                w = csv.writer(f)
                w.writerow(final_headers[:width])
                for r in all_results:
                    w.writerow(r + [''] * (width - len(r)))

            print(f"Saved {len(all_results)} unique rows to {out_csv_filename} (pages {current_chunk_start_page}-{page})")
            google.colab.files.download(out_csv_filename)

            all_results = [] # Reset for next chunk
            current_chunk_start_page = page + 1
            pages_in_current_chunk = 0 # Reset page count for next chunk

        time.sleep(DELAY)

    if all_results: # This handles any remaining rows at the very end
        # Save any remaining results if the loop finished and there's a partial chunk
        out_csv_filename = f"{base_filename}_pages_{current_chunk_start_page}_to_{end_page}.csv"

        width = max(len(r) for r in all_results)
        final_headers = headers or [f'col{i+1}' for i in range(width)]
        if len(final_headers) < width:
            final_headers = final_headers + [f'col{i+1}' for i in range(len(final_headers) + 1, width + 1)]

        with open(out_csv_filename, 'w', encoding='utf-8', newline='') as f:
            w = csv.writer(f)
            w.writerow(final_headers[:width])
            for r in all_results:
                w.writerow(r + [''] * (width - len(r)))

        print(f"Saved {len(all_results)} unique rows to {out_csv_filename} (pages {current_chunk_start_page}-{end_page})")
    #    google.colab.files.download(out_csv_filename)

    if not seen_keys:
        print("No results extracted overall.")

if __name__ == '__main__':
    main()
