import { Injectable } from '@angular/core';
import { HttpEvent, HttpHandler, HttpInterceptor, HttpRequest } from '@angular/common/http';
import { finalize, tap } from 'rxjs/operators';
import { Observable } from 'rxjs';
import { HTTPLoadingEventController } from '../listeners/http-loading-event-controller.service';

/**
 * HTTP interceptor for detecting if the application is currently communicating with the back-end
 */
@Injectable()
export class HTTPLoadingInterceptor implements HttpInterceptor {
  private static readonly LOADING_DELAY = 400;

  /**
   * Create a new instance
   */
  constructor(protected loadingEventController: HTTPLoadingEventController) {
  }

  /**
   * Intercept all HTTP requests
   */
  intercept(req: HttpRequest<undefined>, next: HttpHandler): Observable<HttpEvent<undefined>> {
    return next.handle(req).pipe(
      tap(event => {
        this.loadingEventController.setLoading(true);
        return event;
      }),
      finalize(() => {
        // The status should not be reset immediately!
        setTimeout(() => {
          this.loadingEventController.setLoading(false);
        }, HTTPLoadingInterceptor.LOADING_DELAY);
      })
    );
  }

}
