import { CommonModule } from '@angular/common';
import { HttpClientModule } from '@angular/common/http';
import { NgModule } from '@angular/core';
import { ReactiveFormsModule, FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { AutoCompleteModule } from 'primeng/autocomplete';
import { ButtonModule } from 'primeng/button';
import { CalendarModule } from 'primeng/calendar';
import { CheckboxModule } from 'primeng/checkbox';
import { ConfirmDialogModule } from 'primeng/confirmdialog';
import { ContextMenuModule } from 'primeng/contextmenu';
import { DialogModule } from 'primeng/dialog';
import { DropdownModule} from 'primeng/dropdown';
import { FieldsetModule } from 'primeng/fieldset';
import { FileUploadModule } from 'primeng/fileupload';
import { InputTextModule } from 'primeng/inputtext';
import { InputTextareaModule } from 'primeng/inputtextarea';
import { ListboxModule } from 'primeng/listbox';
import { MessageModule } from 'primeng/message';
import { MessagesModule } from 'primeng/messages';
import { PanelModule } from 'primeng/panel';
import { PasswordModule } from 'primeng/password';
import { PickListModule } from 'primeng/picklist';
import { ProgressBarModule } from 'primeng/progressbar';
import { ScrollPanelModule} from 'primeng/scrollpanel';
import { SliderModule } from 'primeng/slider';
import { TableModule } from 'primeng/table';
import { TabViewModule } from 'primeng/tabview';
import { ToastModule } from 'primeng/toast';
import { TreeModule } from 'primeng/tree';
import { UserSettingsPage } from './pages/settings/user-settings-page';
import { TreeNavigator } from './components/tree-navigator/tree-navigator';
import { SearchInputDialog } from './components/search-input-dialog/search-input-dialog';
import { LovInputField } from './components/lov-input-field/lov-input-field';
import { ErrorDialog } from './components/error-dialog/error-dialog';
import { WelcomePage } from './pages/welcome/welcome-page';
import { MultiSelectionList } from './components/multi-selection-list/multi-selection-list';
import { ElementCollectionEditor } from './components/element-collection-editor/element-collection-editor';
import { NotFoundPage } from './pages/not-found/not-found-page';
import { LoginPage } from './pages/login/login-page';
import { AuthGuardService } from './services/auth-guard.service';
import { ChangePasswordPage } from './pages/change-password/change-password-page';
import { ViewContainer } from './components/view-container/view-container.component';
import { FormControlContainerComponent } from './components/form-control-container/form-control-container.component';
import { FormContainerComponent } from './components/form-container/form-container.component';
import { FormButtonContainerComponent } from './components/form-button-container/form-button-container.component';
import { SaveSearchDialog } from './components/save-search-dialog/save-search-dialog';
import { SavedSearchSelectionDialog } from './components/saved-search-selection-dialog/saved-search-selection-dialog';
import { FormLink } from './components/form-link/formlink.component';
import { MailLink } from './components/mail-link/maillink.component';
import { WebLink } from './components/web-link/weblink.component';
import { DateTimeFormatterDirective } from './directives/date-time-formatter.directive';
import { DateFormatterDirective } from './directives/date-formatter.directive';
import { NumberFormatterDirective } from './directives/number-formatter.directive';
import { ServiceModule } from '../services/service.module';

const routes: Routes = [
  {
    path: 'login',
    component: LoginPage
  },
  {
    path: 'welcome',
    component: WelcomePage,
    canActivate: [AuthGuardService],
    data: {skipPermissionCheck: true}
  },
  {
    path: 'settings',
    component: UserSettingsPage,
    canActivate: [AuthGuardService],
    data: {skipPermissionCheck: true}
  },
  {
    path: 'changepassword',
    component: ChangePasswordPage,
    canActivate: [AuthGuardService],
    data: {skipPermissionCheck: true}
  }
];

@NgModule({
  imports: [
    CommonModule, FormsModule, ReactiveFormsModule, DialogModule, SliderModule,
    ConfirmDialogModule, CheckboxModule, DropdownModule, CalendarModule, FieldsetModule,
    PickListModule, TreeModule, ToastModule, ButtonModule, TableModule, InputTextModule, ScrollPanelModule,
    MessageModule, PasswordModule, ListboxModule, ContextMenuModule, RouterModule.forChild(routes)
  ],
  declarations: [
    WelcomePage, TreeNavigator, UserSettingsPage, SearchInputDialog, ErrorDialog, ViewContainer,
    FormContainerComponent, FormLink, MailLink, WebLink, LovInputField, MultiSelectionList, ElementCollectionEditor, NotFoundPage,
    LoginPage, ChangePasswordPage, FormControlContainerComponent, FormContainerComponent,
    FormButtonContainerComponent, SaveSearchDialog, SavedSearchSelectionDialog, DateTimeFormatterDirective,
    DateFormatterDirective, NumberFormatterDirective
  ],
  exports: [
    CommonModule, FormsModule, ReactiveFormsModule, HttpClientModule, ServiceModule, AutoCompleteModule,
    ButtonModule, CalendarModule, CheckboxModule, ConfirmDialogModule, ContextMenuModule, DialogModule,
    DropdownModule, FieldsetModule, FileUploadModule, InputTextareaModule, InputTextModule, MessageModule,
    MessagesModule, PanelModule, ProgressBarModule, TableModule, TabViewModule, TreeModule, ToastModule,
    ScrollPanelModule, TreeNavigator, SearchInputDialog, LovInputField, MultiSelectionList, ElementCollectionEditor, ViewContainer,
    FormContainerComponent, FormLink, MailLink, WebLink, ErrorDialog, LoginPage, ChangePasswordPage,
    FormControlContainerComponent, FormButtonContainerComponent, SaveSearchDialog, SavedSearchSelectionDialog,
    DateTimeFormatterDirective, DateFormatterDirective, NumberFormatterDirective
  ]
})
export class AppCommonModule { }
