import { TestBed } from '@angular/core/testing';
import { App } from './app';
import { ConfirmationService, MessageService } from 'primeng/api';

describe('App', () => {
  beforeEach(() => TestBed.configureTestingModule({
    imports: [App],
    providers: [
      MessageService,
      ConfirmationService
    ],
  }));

  it('should create the app', () => {
    const fixture = TestBed.createComponent(App);
    const app = fixture.componentInstance;
    expect(app).toBeTruthy();
  });
});
