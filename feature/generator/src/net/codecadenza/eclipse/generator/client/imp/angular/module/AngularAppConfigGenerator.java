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
 * Generator for the application configuration
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AngularAppConfigGenerator extends AbstractTypeScriptSourceGenerator {
	private final Project project;
	private final boolean securityEnabled;
	private final Set<DomainObject> domainObjectsWithForms;

	/**
	 * Constructor
	 * @param project
	 */
	public AngularAppConfigGenerator(Project project) {
		super("The application configuration");

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
		final var path = ANGULAR_APP_FOLDER + "/app.config.ts";

		return new WorkspaceFile(project, BuildArtifactType.GUI, path, null);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.imp.angular.common.AbstractTypeScriptSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importTypes(Stream.of("ApplicationConfig", "importProvidersFrom"), "@angular/core");
		importTypes(Stream.of("Routes", "provideRouter", "withComponentInputBinding"), "@angular/router");
		importTypes(Stream.of("provideHttpClient", "withInterceptorsFromDi"), "@angular/common/http");
		importTypes(Stream.of("ConfirmationService", "MessageService"), "primeng/api");
		importType("Aura", "@primeuix/themes/aura");
		importType("definePreset", "@primeuix/themes");
		importType("providePrimeNG", "primeng/config");
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
		final var loadingInterceptor = "provide: HTTP_INTERCEPTORS, useClass: HTTPLoadingInterceptor, multi: true";

		formatter.addLine("const appRoutes: Routes = [");
		formatter.increaseIndent();
		formatter.addLine("{ path: '', redirectTo: '/welcome', pathMatch: 'full' },");
		formatter.addLine("{ path: '**', component: NotFoundPage }");
		formatter.decreaseIndent();
		formatter.addLine("];");
		formatter.addBlankLine();
		formatter.addLine("const defaultPreset = definePreset(Aura, {");
		formatter.increaseIndent();
		formatter.addLine("semantic: {");
		formatter.increaseIndent();
		formatter.addLine("primary: {");
		formatter.increaseIndent();
		formatter.addLine("50: '{blue.50}',");
		formatter.addLine("100: '{blue.100}',");
		formatter.addLine("500: '{blue.500}',");
		formatter.addLine("600: '{blue.600}'");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("});");
		formatter.addBlankLine();
		formatter.addLine("export const appConfig: ApplicationConfig = {");
		formatter.increaseIndent();
		formatter.addLine("providers: [");
		formatter.increaseIndent();
		formatter.addLine("importProvidersFrom(AppCommonModule),");

		for (final DomainObject domainObject : domainObjectsWithForms) {
			final String domainObjectName = domainObject.getName().toLowerCase();
			final var moduleName = domainObject.getName() + "Module";

			importType(moduleName, "./pages/" + domainObjectName + "/" + domainObjectName + ".module");

			formatter.addLine("importProvidersFrom(" + moduleName + "),");
		}

		formatter.addLine("provideRouter(appRoutes, withComponentInputBinding()),");
		formatter.addLine("provideHttpClient(withInterceptorsFromDi()), [{");
		formatter.increaseIndent();
		formatter.addLine(loadingInterceptor);
		formatter.decreaseIndent();
		formatter.addLine("}" + (securityEnabled ? ", {" : "],"));

		if (securityEnabled) {
			formatter.increaseIndent();

			if (project.isSpringBootApplication())
				formatter.addLine("provide: HTTP_INTERCEPTORS, useClass: HTTPAuthInterceptor, multi: true");
			else
				formatter.addLine("provide: HTTP_INTERCEPTORS, useClass: HTTPBasicAuthInterceptor, multi: true");

			formatter.decreaseIndent();
			formatter.addLine("}],");
		}

		formatter.addLine("providePrimeNG({");
		formatter.increaseIndent();
		formatter.addLine("theme: {");
		formatter.increaseIndent();
		formatter.addLine("preset: defaultPreset");
		formatter.decreaseIndent();
		formatter.addLine("}");
		formatter.decreaseIndent();
		formatter.addLine("}),");
		formatter.addLine("ConfirmationService,");
		formatter.addLine("MessageService");
		formatter.decreaseIndent();
		formatter.addLine("]");
		formatter.decreaseIndent();
	}

}
