import { Component, inject, ViewChild, computed, signal, Signal } from '@angular/core';
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
  protected readonly searchInput = signal<SearchInput | undefined>(undefined);
  protected readonly loading = signal(false);
  protected readonly items = signal<T[]>([]);
  protected readonly selectedItem = signal<T | undefined>(undefined);
  protected readonly showSearchInputDialog = signal(false);
  protected readonly showSaveSearchDialog = signal(false);
  protected readonly showSavedSearchSelectionDialog = signal(false);
  protected readonly summaryText = signal('');
  protected id = '';
  protected enableSearchInput = true;
  protected showNewButton = false;
  protected showImportButton = false;
  protected contextMenuItems: MenuItem[] = [];
  protected title = '';
  protected menuItems: MenuItem[] = [];
  protected readonly columns: Signal<ColumnDefinition[]> = computed(() => {
    // Update the column definition as soon as the search input has changed
    const currentSearch = this.searchInput();

    if (!currentSearch || !currentSearch.searchFields) {
      return [];
    }

    return currentSearch.searchFields
      .filter(field => field.displayInTable)
      .map(field => ({
        field: field.name,
        header: field.label,
        width: field.width + 'px',
        type: field.type,
        dateTimeFormat: field.dateTimeFormat
      }));
  });

  /**
   * Initialize the view and load the data
   */
  init(id: string, enableSearchInput: boolean) {
    this.id = id;
    this.enableSearchInput = enableSearchInput;

    let initialSearch: SearchInput | undefined;

    if (this.enableSearchInput) {
      initialSearch = this.savedSearchService.getLastSearch(this.id);
    }

    this.searchInput.set(initialSearch ?? this.initSearchInput());

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
        command: () => this.showSearchInputDialog.set(true)
      });

      this.menuItems.push({
        label: this.i18n.translate('cmd_save'),
        id: 'cmdSave',
        icon: 'pi pi-star',
        command: () => this.showSaveSearchDialog.set(true)
      });

      this.menuItems.push({
        label: this.i18n.translate('cmd_open'),
        id: 'cmdOpen',
        icon: 'pi pi-list',
        command: () => this.showSavedSearchSelectionDialog.set(true)
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
   * Initialize the table and load data
   */
  refreshView() {
    this.loading.set(true);
    this.dataTable.reset();

    this.loadData().subscribe({
      next: result => this.items.set(result),
      error: error => this.displayError(error, this.i18n.translate('msg_errordataload')),
      complete: () => {
        this.loading.set(false);
        this.summaryText.set(this.i18n.translate('msg_finisheddataload', this.items().length.toString()));
      }
    });
  }

  /**
   * Callback listener that notifies this component to start a search operation
   */
  performSearchOperation(searchInput: SearchInput) {
    this.searchInput.set(searchInput);
    this.showSearchInputDialog.set(false);
    this.showSavedSearchSelectionDialog.set(false);

    console.log('Perform search operation by using: ' + JSON.stringify(searchInput));

    this.refreshView();

    if (this.enableSearchInput) {
      this.savedSearchService.saveLastSearch(this.id, searchInput);
    }
  }

  /**
   * Callback listener that notifies this component to start a count operation
   */
  performCountOperation(searchInput: SearchInput) {
    this.searchInput.set(searchInput);
    this.loading.set(true);
    this.showSearchInputDialog.set(false);

    console.log('Perform count operation by using: ' + JSON.stringify(this.searchInput));

    this.countRecords().subscribe({
      next: countResult  => {
        const message = { severity: 'info', summary: this.i18n.translate('msg_countresult', countResult.toString()) };
        this.messageService.add(message);
      },
      error: error => this.displayError(error, this.i18n.translate('msg_errorcount')),
      complete: () => this.loading.set(false)
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

    this.loading.set(false);
    this.messageService.add({ severity: 'error', summary: errorMsg });
    this.summaryText.set(errorMsg);
  }

  /**
   * Format date and decimal values
   */
  formatField(fieldName: string, value: unknown) {
    if (!value) {
      return value;
    }

    const searchField = this.searchInput()!.getSearchFieldByName(fieldName);

    if (!searchField) {
      console.error(`The field '${fieldName}' could not be found!`);
      return value;
    }

    return this.formatterService.formatField(searchField.type, searchField.dateTimeFormat, value);
  }

}
