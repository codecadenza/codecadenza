import { Component, Input, Output, EventEmitter } from '@angular/core';
import { MessageService, MenuItem, ConfirmationService } from 'primeng/api';
import { FileUploadHandlerEvent, FileUpload } from 'primeng/fileupload';
import { ColumnDefinition, TableDefinition } from '../../model/table-definition.model';
import { FormatterService } from '../../services/formatter.service';
import { I18NService } from '../../services/i18n.service';
import { WindowResizeEventController } from '../../listeners/window-resize-event-controller.service';
import { Observable } from 'rxjs';

/**
 * Abstract base class for data table components
 */
@Component({ template: ''})
export abstract class AbstractDataTable<T> {
  private static readonly DEFAULT_MAX_NUMBER_OF_ITEMS = 1000;
  private static readonly TABLE_STYLE_SMALL = { width: 'calc(100vw - 50px - 7.9rem)' };
  private static readonly TABLE_STYLE_LARGE = { width: 'calc(100vw - 330px - 6.6rem)' };
  @Output() selectItem = new EventEmitter<T | null>();
  @Input() maxNumberOfItems: number;
  @Input() tableId = '';
  @Input() rowKey = '';
  private tableDefinition!: TableDefinition;
  showNewButton = false;
  showImportButton = false;
  summaryText = '';
  loading = false;
  items: Array<T> = [];
  selectedItem?: T;
  columns: ColumnDefinition[] = [];
  contextMenuItems: MenuItem[] = [];
  title = '';
  tableStyle = AbstractDataTable.TABLE_STYLE_LARGE;

  /**
   * Create a new instance
   */
  constructor(protected confirmationService: ConfirmationService,
    protected messageService: MessageService,
    protected i18n: I18NService, protected formatterService: FormatterService,
    protected windowEventController? : WindowResizeEventController) {
    this.maxNumberOfItems = AbstractDataTable.DEFAULT_MAX_NUMBER_OF_ITEMS;

    if (this.windowEventController) {
      this.windowEventController.onWidthThreshold().subscribe(hasSmallWidth => {
        if (hasSmallWidth) {
          this.tableStyle = AbstractDataTable.TABLE_STYLE_SMALL;
        } else {
          this.tableStyle = AbstractDataTable.TABLE_STYLE_LARGE;
        }
      });
    }
  }

  /**
   * Initialize the view
   */
  init() {
    this.tableDefinition = this.initTableDefinition();

    // Add columns
    this.columns = this.tableDefinition.getColumns();
  }

  /**
   * Search for new items and display them in the table
   */
  searchItems(filterString: string) {
    console.log('Search items by using filter: ' + filterString);

    this.loading = true;

    this.loadData(filterString).subscribe({
      next: result => this.items = result.splice(0, this.maxNumberOfItems),
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
   * Method that is invoked to load the data. An implementation class must override this method accordingly!
   */
  abstract loadData(_filterText: string): Observable<Array<T>>;

  /**
   * Initialize the table. An implementation class must override this method accordingly!
   */
  abstract initTableDefinition(): TableDefinition;

  /**
   * Method that is invoked as soon as a user performs a double click on a row in the table
   */
  onRowDoubleClicked(item: T) {
    this.selectItem.emit(item);
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
  openConfirmDeleteDialog(deleteFunction: () => void) {
    if (!this.selectedItem) {
      return;
    }

    this.confirmationService.confirm({
      message: this.i18n.translate('msg_confirmdelete'),
      header: this.i18n.translate('dlg_header_conf'),
      icon: 'pi pi-exclamation-triangle',
      accept: () => deleteFunction()
    });
  }

  /**
   * Open a confirmation dialog before calling the provided copy function
   */
  openConfirmCopyDialog(copyFunction: () => void) {
    if (!this.selectedItem) {
      return;
    }

    this.confirmationService.confirm({
      message: this.i18n.translate('msg_confirmcopy'),
      header: this.i18n.translate('dlg_header_conf'),
      icon: 'pi pi-exclamation-triangle',
      accept: () => copyFunction()
    });
  }

  /**
   * Create an error message and display the error in the summary field
   */
  displayError(error: Error, errorMsg: string) {
    console.log(error);

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

    const columnDefinition = this.tableDefinition.getColumnByFieldName(fieldName);

    if (!columnDefinition) {
      console.error(`The field '${fieldName}' could not be found!`);
      return value;
    }

    return this.formatterService.formatField(columnDefinition.type, columnDefinition.dateTimeFormat, value);
  }

}
