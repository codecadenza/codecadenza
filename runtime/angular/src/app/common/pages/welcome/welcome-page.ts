import { Component } from '@angular/core';
import { ViewContainer } from '../../components/view-container/view-container.component';
import { Fieldset } from 'primeng/fieldset';

/**
 * Page for displaying general information about the generated application
 */
@Component({
  templateUrl: './welcome-page.html',
  imports: [ViewContainer, Fieldset]
})
export class WelcomePage {}
