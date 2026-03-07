import { Component, inject, OnInit } from '@angular/core';
import { UntypedFormGroup, UntypedFormControl, Validators, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { AuthService } from '../../services/auth.service';
import { Bind } from 'primeng/bind';
import { Dialog } from 'primeng/dialog';
import { FormContainerComponent } from '../../components/form-container/form-container.component';
import { FormControlContainerComponent } from '../../components/form-control-container/form-control-container.component';
import { InputText } from 'primeng/inputtext';
import { PasswordDirective } from 'primeng/password';
import { ButtonDirective } from 'primeng/button';

/**
 * Login page
 */
@Component({
  templateUrl: './login-page.html',
  imports: [Bind, Dialog, FormsModule, ReactiveFormsModule, FormContainerComponent, FormControlContainerComponent, InputText,
    PasswordDirective, ButtonDirective]
})
export class LoginPage implements OnInit {
  private readonly authService = inject(AuthService);
  protected formGroup!: UntypedFormGroup;
  protected credentials = { userName: '', password: '' };

  /**
   * Initialize the form
   */
  ngOnInit() {
    this.formGroup = new UntypedFormGroup({
      userName: new UntypedFormControl('', Validators.required),
      password: new UntypedFormControl('', Validators.required)
    });

    this.formGroup.patchValue(this.credentials);
  }

  /**
   * Perform the log in operation
   */
  performLogin() {
    Object.assign(this.credentials, this.formGroup.value);

    this.authService.login(this.credentials.userName, this.credentials.password);
  }

}
