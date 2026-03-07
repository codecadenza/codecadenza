import { Component, inject, Input } from '@angular/core';
import { HttpErrorResponse } from '@angular/common/http';
import { I18NService } from '../../services/i18n.service';
import { NavigationHistoryService } from '../../services/navigation-history.service';
import { Bind } from 'primeng/bind';
import { Dialog } from 'primeng/dialog';
import { PrimeTemplate } from 'primeng/api';
import { ButtonDirective } from 'primeng/button';

/**
 * Dialog that displays errors
 */
@Component({
  selector: '<cc-error-dialog>',
  templateUrl: './error-dialog.html',
  imports: [Bind, Dialog, PrimeTemplate, ButtonDirective]
})
export class ErrorDialog {
  private readonly navigationHistoryService = inject(NavigationHistoryService);
  private readonly i18n = inject(I18NService);
  @Input() public leavePage = false;
  private _error: Error | null = null;
  protected visible = false;
  protected title = '';
  protected details = '';

  /**
   * Return the error
   */
  @Input() public get error() {
    return this._error;
  }

  /**
   * Set the error
   */
  public set error(error: Error | null) {
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
