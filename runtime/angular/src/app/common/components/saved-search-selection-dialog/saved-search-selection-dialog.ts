import { Component, inject, LOCALE_ID, Input, Output, EventEmitter } from '@angular/core';
import { SavedSearchService } from '../../services/saved-search.service';
import { SavedSearch } from '../../model/saved-search.model';
import { SearchInput } from '../../model/search-input.model';
import { Bind } from 'primeng/bind';
import { Dialog } from 'primeng/dialog';
import { FormContainerComponent } from '../form-container/form-container.component';
import { FormControlContainerComponent } from '../form-control-container/form-control-container.component';
import { Listbox } from 'primeng/listbox';
import { FormsModule } from '@angular/forms';
import { PrimeTemplate } from 'primeng/api';
import { ButtonDirective } from 'primeng/button';

/**
 * Dialog for selecting a saved search that should be either executed or deleted
 */
@Component({
  selector: 'cc-saved-search-selection-dialog',
  templateUrl: './saved-search-selection-dialog.html',
  imports: [Bind, Dialog, FormContainerComponent, FormControlContainerComponent, Listbox, FormsModule, PrimeTemplate,
    ButtonDirective]
})
export class SavedSearchSelectionDialog {
  private readonly savedSearchService = inject(SavedSearchService);
  private readonly locale: string = inject(LOCALE_ID);
  @Input() public visible = false;
  @Input() public viewName = '';
  @Input() public searchInput!: SearchInput;
  @Output() private savedSearchSelected = new EventEmitter();
  @Output() private closeDialog = new EventEmitter();
  protected selectedItem: SavedSearch | null = null;
  protected savedSearchObjects: SavedSearch[] = [];
  protected confirmDelete = false;

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
