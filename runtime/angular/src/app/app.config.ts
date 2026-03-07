import { ApplicationConfig, importProvidersFrom } from '@angular/core';
import { Routes, provideRouter, withComponentInputBinding } from '@angular/router';
import { provideHttpClient, withInterceptorsFromDi } from '@angular/common/http';
import { HTTP_INTERCEPTORS } from '@angular/common/http';
import Aura from '@primeuix/themes/aura';
import { providePrimeNG } from 'primeng/config';
import { definePreset } from '@primeuix/themes';
import { MessageService, ConfirmationService } from 'primeng/api';
import { HTTPLoadingInterceptor } from './common/interceptors/http-loading-interceptor.service';
import { NotFoundPage } from './common/pages/not-found/not-found-page';
import { AppCommonModule } from './common/app-common.module';

const appRoutes: Routes = [
  { path: '', redirectTo: '/welcome', pathMatch: 'full' },
  { path: '**', component: NotFoundPage },
];

const defaultPreset = definePreset(Aura, {
  semantic: {
    primary: {
      50: '{blue.50}',
      100: '{blue.100}',
      500: '{blue.500}',
      600: '{blue.600}'
    }
  }
});

export const appConfig: ApplicationConfig = {
  providers: [
    importProvidersFrom(AppCommonModule),
    provideRouter(appRoutes, withComponentInputBinding()),
    provideHttpClient(withInterceptorsFromDi()), {
      provide: HTTP_INTERCEPTORS, useClass: HTTPLoadingInterceptor, multi: true
    },
    providePrimeNG({
      theme: {
        preset: defaultPreset
      }
    }),
    MessageService,
    ConfirmationService
  ]
};
