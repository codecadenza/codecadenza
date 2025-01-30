import { BrowserModule } from '@angular/platform-browser';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { HTTP_INTERCEPTORS } from '@angular/common/http';
import { AppComponent } from './app.component';
import { MessageService, ConfirmationService } from 'primeng/api';
import { AppCommonModule } from './common/app-common.module';
import { NotFoundPage } from './common/pages/not-found/not-found-page';
import { HTTPLoadingInterceptor } from './common/interceptors/http-loading-interceptor.service';

const appRoutes: Routes = [
  { path: '', redirectTo: '/welcome', pathMatch: 'full' },
  { path: '**', component: NotFoundPage }
];

/**
 * The application's main module
 */
@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserAnimationsModule, BrowserModule, AppCommonModule,
    RouterModule.forRoot(appRoutes)
  ],
  providers: [
    MessageService, ConfirmationService,
    { provide: HTTP_INTERCEPTORS, useClass: HTTPLoadingInterceptor, multi: true }
  ],
  bootstrap: [AppComponent]
})
export class AppModule {

}
