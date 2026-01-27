/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.eclipse.generator.client.imp.angular.module;

import static net.codecadenza.eclipse.shared.Constants.ANGULAR_APP_FOLDER;

import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;

/**
 * <p>
 * Generator for the root module of an application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularAppModuleGenerator extends AbstractTypeScriptSourceGenerator {
	private static final int MAX_MODULES_PER_LINE = 4;

	private final Project project;
	private final boolean securityEnabled;
	private final Set<DomainObject> domainObjectsWithForms;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularAppModuleGenerator(Project project) {
		super("The application's main module");

		this.project = project;
		this.securityEnabled = project.getApplicationLogOnDTO() != null;
		this.domainObjectsWithForms = project.getAllFormsOfProject().stream().map(Form::getDomainObject).collect(Collectors.toSet());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#getSourceFile()
	 */
	@Override
	public WorkspaceFile getSourceFile() {
		final var path = ANGULAR_APP_FOLDER + "/app.module.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importType("NgModule", "@angular/core");
		importTypes(Stream.of("ConfirmationService", "MessageService"), "primeng/api");
		importTypes(Stream.of("RouterModule", "Routes"), "@angular/router");
		importType("BrowserModule", "@angular/platform-browser");
		importType("BrowserAnimationsModule", "@angular/platform-browser/animations");
		importType("AppComponent", "./app.component");
		importType("AppCommonModule", "./common/app-common.module");
		importType("NotFoundPage", "./common/pages/not-found/not-found-page");
		importType("HTTP_INTERCEPTORS", "@angular/common/http");
		importType("HTTPLoadingInterceptor", "./common/interceptors/http-loading-interceptor.service");

		if (securityEnabled) {
			if (project.isSpringBootApplication())
				importType("HTTPAuthInterceptor", "./common/interceptors/http-auth-interceptor.service");
			else
				importType("HTTPBasicAuthInterceptor", "./common/interceptors/http-basic-auth-interceptor.service");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#
	 * addTypeDeclaration(net.codecadenza.eclipse.generator.client.imp.angular.common.AngularContentFormatter)
	 */
	@Override
	protected void addTypeDeclaration(AngularContentFormatter formatter) {
		final var loadingInterceptor = "{ provide: HTTP_INTERCEPTORS, useClass: HTTPLoadingInterceptor, multi: true }";
		int moduleCounter = 0;
		var modules = new StringBuilder();

		formatter.addLine("const appRoutes: Routes = [");
		formatter.increaseIndent();
		formatter.addLine("{ path: '', redirectTo: '/welcome', pathMatch: 'full' },");
		formatter.addLine("{ path: '**', component: NotFoundPage }");
		formatter.decreaseIndent();
		formatter.addLine("];");
		formatter.addBlankLine();
		formatter.addLine("@NgModule({");
		formatter.increaseIndent();
		formatter.addLine("declarations: [");
		formatter.increaseIndent();
		formatter.addLine("AppComponent");
		formatter.decreaseIndent();
		formatter.addLine("],");
		formatter.addLine("imports: [");
		formatter.increaseIndent();
		formatter.addLine("BrowserAnimationsModule, BrowserModule, AppCommonModule,");

		for (final DomainObject domainObject : domainObjectsWithForms) {
			final String domainObjectName = domainObject.getName().toLowerCase();
			final var moduleName = domainObject.getName() + "Module";

			importType(moduleName, "./pages/" + domainObjectName + "/" + domainObjectName + ".module");

			modules.append(moduleName + ", ");

			moduleCounter++;

			// Limit the number of modules per line!
			if (moduleCounter == MAX_MODULES_PER_LINE) {
				formatter.addLine(modules.toString().trim());
				modules = new StringBuilder();
				moduleCounter = 0;
			}
		}

		if (!modules.isEmpty())
			formatter.addLine(modules.toString().trim());

		formatter.addLine("RouterModule.forRoot(appRoutes)");
		formatter.decreaseIndent();
		formatter.addLine("],");
		formatter.addLine("providers: [");
		formatter.increaseIndent();
		formatter.addLine("MessageService, ConfirmationService,");
		formatter.addLine(loadingInterceptor + (securityEnabled ? "," : ""));

		if (securityEnabled) {
			if (project.isSpringBootApplication())
				formatter.addLine("{ provide: HTTP_INTERCEPTORS, useClass: HTTPAuthInterceptor, multi: true }");
			else
				formatter.addLine("{ provide: HTTP_INTERCEPTORS, useClass: HTTPBasicAuthInterceptor, multi: true }");
		}

		formatter.decreaseIndent();
		formatter.addLine("],");
		formatter.addLine("bootstrap: [AppComponent]");
		formatter.decreaseIndent();
		formatter.addLine("})");
		formatter.addLine("export class AppModule {");
	}

}
