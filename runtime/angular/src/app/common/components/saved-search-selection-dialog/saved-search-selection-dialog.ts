import { Component, Inject, LOCALE_ID, Input, Output, EventEmitter } from '@angular/core';
import { SavedSearchService } from '../../services/saved-search.service';
import { SavedSearch } from '../../model/saved-search.model';
import { SearchInput } from '../../model/search-input.model';
import { I18NService } from '../../services/i18n.service';

/**
 * Dialog for selecting a saved search that should be either executed or deleted
 */
@Component({
  selector: 'cc-saved-search-selection-dialog',
  templateUrl: './saved-search-selection-dialog.html'
})
export class SavedSearchSelectionDialog {
  @Input() visible = false;
  @Input() viewName = '';
  @Input() searchInput!: SearchInput;
  @Output() savedSearchSelected = new EventEmitter();
  @Output() closeDialog = new EventEmitter();
  selectedItem: SavedSearch | null = null;
  savedSearchObjects: SavedSearch[] = [];
  confirmDelete = false;

  /**
   * Create a new instance
   */
  constructor(protected savedSearchService: SavedSearchService, protected i18n: I18NService,
    @Inject(LOCALE_ID) protected locale: string) {
  }

  /**
   * Load the available saved search objects of the given view
   */
  initDialog() {
    this.savedSearchObjects = this.savedSearchService.getSavedSearchObjectsOfView(this.viewName);
  }

  /**
   * Run the saved search
   */
  runSavedSearch(): boolean {
    if (!this.selectedItem) {
      return false;
    }

    this.visible = false;

    Object.assign(this.searchInput, this.selectedItem.searchInput);

    this.savedSearchSelected.emit(this.searchInput.prepareFilterCriteria(this.locale));
    return true;
  }

  /**
   * Delete the selected saved search
   */
  deleteSavedSearch() {
    if (!this.selectedItem) {
      return;
    }

    this.savedSearchService.deleteSearch(this.selectedItem);

    // Refresh the list box
    this.savedSearchObjects = this.savedSearchService.getSavedSearchObjectsOfView(this.viewName);

    // Reset the selected item and the confirm delete flag
    this.selectedItem = null;
    this.confirmDelete = false;
  }

  /**
   * Notify the parent component that this dialog will be closed
   */
  close() {
    this.confirmDelete = false;

    this.closeDialog.emit();
  }

}
