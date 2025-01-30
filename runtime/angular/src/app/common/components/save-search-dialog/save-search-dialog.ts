import { Component, Input, EventEmitter, Output } from '@angular/core';
import { SearchInput } from '../../model/search-input.model';
import { SavedSearch } from '../../model/saved-search.model';
import { SavedSearchService } from '../../services/saved-search.service';

/**
 * Dialog for saving the current search settings
 */
@Component({
  selector: 'cc-save-search-dialog',
  templateUrl: './save-search-dialog.html'
})
export class SaveSearchDialog {
  @Output() closeDialog = new EventEmitter();
  @Input() searchInput!: SearchInput;
  @Input() viewName = '';
  @Input() visible = false;
  name = '';

  /**
   * Create a new instance
   */
  constructor(protected savedSearchService: SavedSearchService) {
  }

  /**
   * Save the search object
   */
  save() {
    const savedSearch = new SavedSearch();
    savedSearch.searchInput = this.searchInput;
    savedSearch.viewName = this.viewName;
    savedSearch.name = this.name;

    this.savedSearchService.saveSearch(savedSearch);

    this.visible = false;
    this.name = '';
  }

  /**
   * Notify the parent component that this dialog will be closed
   */
  close() {
    this.closeDialog.emit();
  }

}
