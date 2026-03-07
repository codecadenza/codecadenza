import { Component, AfterViewInit, inject, signal } from '@angular/core';
import { AsyncPipe } from '@angular/common';
import { RouterOutlet } from '@angular/router';
import { startWith, tap, delay } from 'rxjs/operators';
import { ToastModule } from 'primeng/toast';
import { ConfirmDialogModule } from 'primeng/confirmdialog';
import { ProgressBarModule } from 'primeng/progressbar';
import { AppCommonModule } from './common/app-common.module';
import { HTTPLoadingEventController } from './common/listeners/http-loading-event-controller.service';
import { WindowResizeEventController } from './common/listeners/window-resize-event-controller.service';
import { AuthService } from './common/services/auth.service';
import { ComponentScalingService } from './common/services/component-scaling.service';
import { NavigationHistoryService } from './common/services/navigation-history.service';

@Component({
  selector: 'cc-root',
  imports: [ RouterOutlet, AsyncPipe, AppCommonModule, ToastModule, ConfirmDialogModule, ProgressBarModule],
  templateUrl: './app.html'
})
export class App implements AfterViewInit {
  protected loading = signal<boolean>(false);
  protected collapseNavigator = signal<boolean>(false);
  protected readonly loadingEventController = inject(HTTPLoadingEventController);
  protected readonly windowEventController = inject(WindowResizeEventController);
  protected readonly componentScalingService = inject(ComponentScalingService);
  protected readonly navigationHistoryService = inject(NavigationHistoryService);
  protected readonly authService = inject(AuthService);

  loggedIn$ = this.authService.onLoginStatusChanged();

  /**
   * Create a new instance of this component
   */
  constructor() {
    this.navigationHistoryService.startTracking();
  }

  /**
   * Start listening for HTTP loading events shortly after the view has been initialized
   */
  ngAfterViewInit() {
    this.componentScalingService.initComponentScaling();

    this.loadingEventController.onLoadingStatusChanged()
      .pipe(
        startWith(false), 
        delay(0), 
        tap(status => this.loading.set(status === true))
      )
      .subscribe();

    this.windowEventController.onWidthThreshold().subscribe(hasSmallWidth => {
      this.collapseNavigator.set(hasSmallWidth);
    });
  }

}
