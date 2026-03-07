import { Component, inject, OnInit } from '@angular/core';
import { Validators, FormGroup, FormControl, FormsModule, ReactiveFormsModule } from '@angular/forms';
import { AuthService } from '../../services/auth.service';
import { ChangePassword } from '../../model/change-password.model';
import { ViewContainer } from '../../components/view-container/view-container.component';
import { FormContainerComponent } from '../../components/form-container/form-container.component';
import { FormControlContainerComponent } from '../../components/form-control-container/form-control-container.component';
import { Bind } from 'primeng/bind';
import { PasswordDirective } from 'primeng/password';
import { ButtonDirective } from 'primeng/button';

/**
 * Page for changing the user's password
 */
@Component({
  templateUrl: './change-password-page.html',
  imports: [ViewContainer, FormsModule, ReactiveFormsModule, FormContainerComponent, FormControlContainerComponent, Bind,
    PasswordDirective, ButtonDirective]
})
export class ChangePasswordPage implements OnInit {
  private readonly authService = inject(AuthService);
  protected formGroup!: FormGroup;
  protected passwords!: ChangePassword;

  /**
   * Initialize the form
   */
  ngOnInit() {
    this.formGroup = new FormGroup({
      oldPassword: new FormControl('', Validators.required),
      newPassword: new FormControl('', Validators.required),
      confirmedPassword: new FormControl('', Validators.required)
    });

    this.passwords = {
      oldPassword: '',
      newPassword: '',
      confirmedPassword: ''
    };

    this.formGroup.patchValue(this.passwords);
  }

  /**
   * Save the new password
   */
  savePassword() {
    Object.assign(this.passwords, this.formGroup.value);

    this.authService.changePassword(this.passwords.oldPassword, this.passwords.newPassword, this.passwords.confirmedPassword);
  }

}
