import { SearchInput } from './search-input.model';

/**
 * Domain object for saving search input objects
 */
export class SavedSearch {
  // The current time in milliseconds should be sufficient for the uniqueness of an object!
  id = new Date().getTime();
  viewName = '';
  name = '';
  searchInput = new SearchInput();
  lastSearch = false;
}
