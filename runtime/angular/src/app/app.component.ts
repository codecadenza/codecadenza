import { Component, AfterViewInit } from '@angular/core';
import { Observable } from 'rxjs';
import { startWith, tap, delay } from 'rxjs/operators';
import { HTTPLoadingEventController } from './common/listeners/http-loading-event-controller.service';
import { WindowResizeEventController } from './common/listeners/window-resize-event-controller.service';
import { AuthService } from './common/services/auth.service';
import { ComponentScalingService } from './common/services/component-scaling.service';
import { NavigationHistoryService } from './common/services/navigation-history.service';

/**
 * Main component of this application
 */
@Component({
  selector: 'cc-root',
  templateUrl: './app.component.html'
})
export class AppComponent implements AfterViewInit {
  loading = false;
  loggedIn$: Observable<boolean>;
  collapseNavigator = false;

  /**
   * Create a new instance of this component
   */
  constructor(private readonly loadingEventController: HTTPLoadingEventController,
    private readonly windowEventController: WindowResizeEventController,
    protected componentScalingService: ComponentScalingService,
    protected navigationHistoryService: NavigationHistoryService,
    protected authService: AuthService) {
    this.loggedIn$ = this.authService.onLoginStatusChanged();
    this.navigationHistoryService.startTracking();
  }

  /**
   * Start listening for HTTP loading events shortly after the view has been initialized
   */
  ngAfterViewInit() {
    this.componentScalingService.initComponentScaling();

    this.loadingEventController.onLoadingStatusChanged()
      .pipe(startWith(null), delay(0), tap(status => this.loading = status === true))
      .subscribe();

    this.windowEventController.onWidthThreshold().subscribe(hasSmallWidth => {
      this.collapseNavigator = hasSmallWidth;
    });
  }

  /**
   * Logout the user
   */
  logout() {
    this.authService.logout();
  }

}
