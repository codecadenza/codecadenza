import { TestBed } from '@angular/core/testing';
import { AppComponent } from './app.component';
import { AppCommonModule } from './common/app-common.module';
import { AppModule } from './app.module';
import { ConfirmationService, MessageService } from 'primeng/api';

describe('AppComponent', () => {
  beforeEach(() => TestBed.configureTestingModule({
    declarations: [AppComponent],
    providers: [
      AppModule,
      MessageService,
      ConfirmationService
    ],
    imports: [
      AppCommonModule
    ]
  }));

  it('should create the app', () => {
    const fixture = TestBed.createComponent(AppComponent);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });
});
