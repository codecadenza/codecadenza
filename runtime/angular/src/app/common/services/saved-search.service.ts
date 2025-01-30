import { Injectable } from '@angular/core';
import { SearchInput } from '../model/search-input.model';
import { SavedSearch } from '../model/saved-search.model';

/**
 * Service for search objects that are stored in the browser's local storage
 */
@Injectable({ providedIn: 'root' })
export class SavedSearchService {
  private static readonly STORAGE_KEY = 'SAVED_SEARCH_OBJECTS$';

  /**
   * Save the given search object
   */
  saveSearch(savedSearch: SavedSearch) {
    const savedSearchObjects: SavedSearch[] = this.getSavedSearchObjects();

    savedSearchObjects.push(savedSearch);

    this.saveInLocalStorage(savedSearchObjects);
  }

  /**
   * Get all saved search objects of the given view
   */
  getSavedSearchObjectsOfView(viewName: string): SavedSearch[] {
    const savedSearchObjects: SavedSearch[] = this.getSavedSearchObjects();

    return savedSearchObjects.filter(obj => obj.viewName === viewName && obj.lastSearch === false).sort(this.sortObjects);
  }

  /**
   * Get the last saved search object of the given view
   */
  getLastSavedSearchObject(viewName: string): SavedSearch | undefined {
    const savedSearchObjects: SavedSearch[] = this.getSavedSearchObjects();

    return savedSearchObjects.find(obj => obj.viewName === viewName && obj.lastSearch === true);
  }

  /**
   * Get the last search of the given view
   */
  getLastSearch(viewName: string): SearchInput | undefined {
    const lastSavedSearch = this.getLastSavedSearchObject(viewName);

    if (!lastSavedSearch) {
      return undefined;
    }

    return Object.assign(new SearchInput(), lastSavedSearch.searchInput);
  }

  /**
   * Save the last search of the given view
   */
  saveLastSearch(viewName: string, searchInput: SearchInput) {
    const newLastSearch = new SavedSearch;
    newLastSearch.viewName = viewName;
    newLastSearch.lastSearch = true;
    newLastSearch.searchInput = searchInput;

    // Search for the previous last search and delete it
    const previousLastSearch = this.getLastSavedSearchObject(viewName);

    if (previousLastSearch) {
      this.deleteSearch(previousLastSearch);
    }

    const savedSearchObjects: SavedSearch[] = this.getSavedSearchObjects();
    savedSearchObjects.push(newLastSearch);

    this.saveInLocalStorage(savedSearchObjects);
  }

  /**
   * Delete the given saved search object
   */
  deleteSearch(savedSearch: SavedSearch) {
    let savedSearchObjects: SavedSearch[] = this.getSavedSearchObjects();

    savedSearchObjects = savedSearchObjects.filter(obj => obj.id !== savedSearch.id);

    this.saveInLocalStorage(savedSearchObjects);
  }

  /**
   * Return all saved search objects from the local storage
   */
  private getSavedSearchObjects(): SavedSearch[] {
    const savedSearchString = localStorage.getItem(SavedSearchService.STORAGE_KEY);

    if (!savedSearchString) {
      return [];
    }

    return JSON.parse(savedSearchString);
  }

  /**
   * Save the search objects in the local storage
   */
  private saveInLocalStorage(objects: SavedSearch[]) {
    localStorage.setItem(SavedSearchService.STORAGE_KEY, JSON.stringify(objects));
  }

  /**
   * Sort two search objects alphabetically by using their names
   */
  private sortObjects(savedSearchLeft: SavedSearch, savedSearchRight: SavedSearch): number {
    if (savedSearchLeft.name < savedSearchRight.name) {
      return -1;
    }

    if (savedSearchLeft.name > savedSearchRight.name) {
      return 1;
    }

    return 0;
  }

}
