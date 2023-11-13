module ParserGenerator.CodeGen.CodeModel

type TypeSignature =
    | PlainType of string
    | GenericInstanceType of string * TypeSignature list

type RecordField =
    { name: string
      type_: TypeSignature }

type RecordDeclaration =
    { name: string
      fields: RecordField list }

type SumTypeConstructor =
    { name: string
      type_: TypeSignature option }

type SumTypeDeclaration =
    { name: string
      constructors: SumTypeConstructor list }

type TypeAliasDeclaration =
    { name: string
      aliasedType: TypeSignature }

type TypeDeclaration = // TODO access modifier
    | Record of RecordDeclaration
    | SumType of SumTypeDeclaration
    | Alias of TypeAliasDeclaration

type OptionalType =
    | Explicit of TypeSignature
    | Implicit

type Expression =
    | UseBinding of string * Expression
    | MethodInvocation of Expression * string * Expression list
    | Identifier of string

type BindingParameter =
    { name: string
      type_: OptionalType }

type BindingDefinition =
    { name: string
      parameters: BindingParameter list
      type_: OptionalType
      body: Expression }

type ModuleMember =
    | Type of TypeDeclaration
    | Binding of BindingDefinition

type ModuleAccessModifier =
    | Default
    | Public
    | Internal
    | Private

type Module = // TODO should be inside a namespace
    { fullName: string
      accessModifier: ModuleAccessModifier
      recursive : bool
      members: ModuleMember list }
