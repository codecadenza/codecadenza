import { Component, Input, EventEmitter, Output, inject } from '@angular/core';
import { SearchInput } from '../../model/search-input.model';
import { SavedSearch } from '../../model/saved-search.model';
import { SavedSearchService } from '../../services/saved-search.service';
import { Bind } from 'primeng/bind';
import { Dialog } from 'primeng/dialog';
import { FormContainerComponent } from '../form-container/form-container.component';
import { FormControlContainerComponent } from '../form-control-container/form-control-container.component';
import { FormsModule } from '@angular/forms';
import { InputText } from 'primeng/inputtext';
import { PrimeTemplate } from 'primeng/api';
import { ButtonDirective } from 'primeng/button';

/**
 * Dialog for saving the current search settings
 */
@Component({
  selector: 'cc-save-search-dialog',
  templateUrl: './save-search-dialog.html',
  imports: [Bind, Dialog, FormContainerComponent, FormControlContainerComponent, FormsModule, InputText, PrimeTemplate,
    ButtonDirective]
})
export class SaveSearchDialog {
  private readonly savedSearchService = inject(SavedSearchService);	
  @Output() private closeDialog = new EventEmitter();
  @Input() public searchInput!: SearchInput;
  @Input() public viewName = '';
  @Input() public visible = false;
  protected name = '';

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
