import { Component, inject } from '@angular/core';
import { Router } from '@angular/router';
import { Bind } from 'primeng/bind';
import { Dialog } from 'primeng/dialog';
import { PrimeTemplate } from 'primeng/api';
import { ButtonDirective } from 'primeng/button';

/**
 * Page that is displayed if the requested resource could not be found
 */
@Component({
  templateUrl: './not-found-page.html',
  imports: [Bind, Dialog, PrimeTemplate, ButtonDirective]
})
export class NotFoundPage {
  protected readonly router = inject(Router);
  protected visible = true;

  /**
   * Navigate to the start page if the dialog is closed
   */
  onCloseDialog() {
    this.router.navigate(['/']);
  }

}
