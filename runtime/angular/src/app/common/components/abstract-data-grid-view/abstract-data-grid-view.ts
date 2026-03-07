import { Component, inject, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import { Observable, of } from 'rxjs';
import { MenuItem, MessageService, ConfirmationService } from 'primeng/api';
import { Table } from 'primeng/table';
import { FileUploadHandlerEvent, FileUpload } from 'primeng/fileupload';
import { ColumnDefinition } from '../../model/table-definition.model';
import { SearchInput } from '../../model/search-input.model';
import { I18NService } from '../../services/i18n.service';
import { FormatterService } from '../../services/formatter.service';
import { SavedSearchService } from '../../services/saved-search.service';

/**
 * Abstract base class for all data grid views
 */
@Component({
  template: ''
})
export abstract class AbstractDataGridView<T> {
  protected readonly router = inject(Router);
  protected readonly confirmationService = inject(ConfirmationService);
  protected readonly messageService = inject(MessageService);
  protected readonly formatterService = inject(FormatterService);
  protected readonly savedSearchService = inject(SavedSearchService);
  protected readonly i18n = inject(I18NService);
  @ViewChild('dataTable', {static: true}) protected dataTable!: Table;
  @ViewChild('fileUpload') protected fileUpload!: FileUpload;
  protected id = '';
  protected enableSearchInput = true;
  protected searchInput!: SearchInput;
  protected showNewButton = false;
  protected showImportButton = false;
  protected showSearchInputDialog = false;
  protected showSaveSearchDialog = false;
  protected showSavedSearchSelectionDialog = false;
  protected summaryText = '';
  protected contextMenuItems: MenuItem[] = [];
  protected loading = false;
  protected items: T[] = [];
  protected selectedItem?: T;
  protected columns: ColumnDefinition[] = [];
  protected title = '';
  protected menuItems: MenuItem[] = [];

  /**
   * Initialize the view and load the data
   */
  init(id: string, enableSearchInput: boolean) {
    this.id = id;
    this.enableSearchInput = enableSearchInput;

    if (this.enableSearchInput) {
      const lastSearch = this.savedSearchService.getLastSearch(this.id);

      if (lastSearch) {
        this.searchInput = lastSearch;
      }
    }

    if (!this.searchInput) {
      this.searchInput = this.initSearchInput();
    }

    this.refreshView();
  }

  /**
   * Add the items to the menu
   */
  addMenuItems() {
    this.menuItems = [];

    if (this.enableSearchInput) {
      this.menuItems.push({
        label: this.i18n.translate('cmd_search'),
        id: 'cmdSearch',
        icon: 'pi pi-search',
        command: () => this.showSearchInputDialog = true
      });

      this.menuItems.push({
        label: this.i18n.translate('cmd_save'),
        id: 'cmdSave',
        icon: 'pi pi-star',
        command: () => this.showSaveSearchDialog = true
      });

      this.menuItems.push({
        label: this.i18n.translate('cmd_open'),
        id: 'cmdOpen',
        icon: 'pi pi-list',
        command: () => this.showSavedSearchSelectionDialog = true
      });
    }

    this.menuItems.push({
      label: this.i18n.translate('cmd_refresh'),
      id: 'cmdRefresh',
      icon: 'pi pi-refresh',
      command: () => this.refreshView()
    });

    this.menuItems.push({
      label: this.i18n.translate('cmd_export'),
      id: 'cmdExport',
      icon: 'pi pi-download',
      command: () => this.dataTable?.exportCSV()
    });

    if (this.showNewButton) {
      this.menuItems.push({
        label: this.i18n.translate('cmd_new'),
        id: 'cmdNew',
        icon: 'pi pi-plus',
        command: () => this.onNewButtonClicked()
      });
    }

    if (this.showImportButton) {
      this.menuItems.push({
        label: this.i18n.translate('cmd_import'),
        id: 'cmdImport',
        icon: 'pi pi-upload',
        command: () => this.fileUpload?.basicFileInput?.nativeElement.click()
      });
    }
  }

  /**
   * Initialize the table by adding all visible columns that are defined by the corresponding search input object
   */
  initTable() {
    this.dataTable.reset();

    // Add columns dynamically
    this.columns = [];

    for (const searchInputField of this.searchInput.searchFields) {
      // Skip invisible columns
      if (!searchInputField.displayInTable) {
        continue;
      }

      const column: ColumnDefinition = {
        field: searchInputField.name,
        header: searchInputField.label,
        width: searchInputField.width + 'px',
        type: searchInputField.type,
        dateTimeFormat: searchInputField.dateTimeFormat
      };

      this.columns.push(column);
    }
  }

  /**
   * Initialize the table and load data
   */
  refreshView() {
    this.loading = true;

    this.initTable();

    this.loadData().subscribe({
      next: result => this.items = result,
      error: error => {
        this.loading = false;
        this.displayError(error, this.i18n.translate('msg_errordataload'));
      },
      complete: () => {
        this.loading = false;
        this.summaryText = this.i18n.translate('msg_finisheddataload', this.items.length.toString());
      }
    });
  }

  /**
   * Callback listener that notifies this component to start a search operation
   */
  performSearchOperation(searchInput: SearchInput) {
    this.searchInput = searchInput;
    this.showSearchInputDialog = false;
    this.showSavedSearchSelectionDialog = false;

    console.log('Perform search operation by using: ' + JSON.stringify(this.searchInput));

    this.refreshView();

    if (this.enableSearchInput) {
      this.savedSearchService.saveLastSearch(this.id, this.searchInput);
    }
  }

  /**
   * Callback listener that notifies this component to start a count operation
   */
  performCountOperation(searchInput: SearchInput) {
    this.searchInput = searchInput;
    this.loading = true;
    this.showSearchInputDialog = false;

    console.log('Perform count operation by using: ' + JSON.stringify(this.searchInput));

    this.countRecords().subscribe({
      next: countResult  => {
        const message = { severity: 'info', summary: this.i18n.translate('msg_countresult', countResult.toString()) };
        this.messageService.add(message);
      },
      error: error => this.displayError(error, this.i18n.translate('msg_errorcount')),
      complete: () => this.loading = false
    });
  }

  /**
   * Perform the count operation and return the result asynchronously.
   * If the implementation class needs the count functionality it must override this method!
   */
  countRecords(): Observable<number> {
    return of(0);
  }

  /**
   * Method that is invoked to load the data.
   * An implementation class must override this method accordingly!
   */
  abstract loadData(): Observable<T[]>;

  /**
   * Initialize a search input object.
   * An implementation class must override this method accordingly!
   */
  abstract initSearchInput(): SearchInput;

  /**
   * Method that is invoked as soon as a user performs a double click on a row in the table.
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  onRowDoubleClicked(_item: T) {
    // This method might be overwritten by subclasses!
  }

  /**
   * Method that is invoked as soon as the user presses the 'New' button.
   */
  onNewButtonClicked() {
    // This method might be overwritten by subclasses!
  }

  /**
   * Method that is invoked as soon as the user presses the 'Import' button.
   */
  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  onImportButtonPressed(_$event: FileUploadHandlerEvent, _fileUpload: FileUpload) {
    // This method might be overwritten by subclasses!
  }

  /**
   * Add an item to the context menu
   */
  addContextMenuItem(label: string, icon: string, id: string, command: () => void) {
    const menuItem = { label: label, icon: icon, id: id, command: command };

    this.contextMenuItems.push(menuItem);
  }

  /**
   * Open a confirmation dialog before calling the provided delete function
   */
  openConfirmDeleteDialog(deleteFunction: (item: T) => void) {
    this.confirmationService.confirm({
      message: this.i18n.translate('msg_confirmdelete'),
      header: this.i18n.translate('dlg_header_conf'),
      icon: 'pi pi-exclamation-triangle',
      accept: deleteFunction
    });
  }

  /**
   * Open a confirmation dialog before calling the provided copy function
   */
  openConfirmCopyDialog(copyFunction: (item: T) => void) {
    this.confirmationService.confirm({
      message: this.i18n.translate('msg_confirmcopy'),
      header: this.i18n.translate('dlg_header_conf'),
      icon: 'pi pi-exclamation-triangle',
      accept: copyFunction
    });
  }

  /**
   * Create an error message and display the error in the summary field
   */
  displayError(error: Error, errorMsg: string) {
    console.error(error);

    this.loading = false;
    this.messageService.add({ severity: 'error', summary: errorMsg });
    this.summaryText = errorMsg;
  }

  /**
   * Format date and decimal values
   */
  formatField(fieldName: string, value: unknown) {
    if (!value) {
      return value;
    }

    const searchField = this.searchInput.getSearchFieldByName(fieldName);

    if (!searchField) {
      console.error(`The field '${fieldName}' could not be found!`);
      return value;
    }

    return this.formatterService.formatField(searchField.type, searchField.dateTimeFormat, value);
  }

}
