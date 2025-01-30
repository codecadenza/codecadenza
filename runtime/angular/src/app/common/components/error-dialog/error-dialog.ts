import { Component, Input } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { I18NService } from '../../services/i18n.service';
import { NavigationHistoryService } from '../../services/navigation-history.service';

/**
 * Dialog that displays errors
 */
@Component({
  selector: '<cc-error-dialog>',
  templateUrl: './error-dialog.html'
})
export class ErrorDialog {
  @Input() leavePage = false;
  private _error: Error | null = null;
  visible = false;
  title = '';
  details = '';

  /**
   * Create a new instance
   */
  constructor(protected i18n: I18NService, private readonly navigationHistoryService: NavigationHistoryService) {
  }

  /**
   * Return the error
   */
  @Input() get error() {
    return this._error;
  }

  /**
   * Set the error
   */
  set error(error: Error | null) {
    this._error = error;

    if (!error) {
      this.visible = false;
      return;
    }

    console.error(error);

    // Extract all necessary information from the provided error object
    if (error instanceof HttpErrorResponse) {
      if (!navigator.onLine) {
        this.title = this.i18n.translate('msg_noconnection');
      } else {
        switch (error.status) {
          case 401: {
            this.title = this.i18n.translate('msg_notauthorized');
            break;
          }
          case 403: {
            this.title = this.i18n.translate('msg_authenticationrequired');
            break;
          }
          case 404: { this.title = this.i18n.translate('msg_resourcenotfound');
            break;
          }
          default: {
            this.title = this.i18n.translate('msg_internalservererror');
            break;
          }
        }
      }
    } else {
      this.title = this.i18n.translate('errordialog_title');
    }

    this.visible = true;
    this.details = error.message;
  }

  /**
   * Listener that is called as soon as the dialog will be closed
   */
  onCloseDialog() {
    if (this.leavePage) {
      this.navigationHistoryService.navigateToPreviousPage();
    }
  }

}
